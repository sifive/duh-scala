'use strict';

const indent = require('./indent');
const paramSchema = require('./param-schema.js');

// ACLK
// ACLKEN
// ARESETn

/*
    AXI4 to Scala adapter specification
*/

const masterAdapterGen = comp => () => {
  const maxId = 1 << 'c.mIDBits';
  const isAligned = false;

  return `AXI4MasterNode(Seq(
    AXI4MasterPortParameters(
      masters = Seq(
        AXI4MasterParameters(
          name    = "${comp.name}",
          id      = IdRange(0, ${maxId}),
          aligned = ${isAligned}
        )
      )
    )
  ))
`;
};

const slaveAdapterGen = comp => e => {
  const params = `c.${e.name}Params`;
  const portMaps = e.abstractionTypes.find(e => e.viewRef === 'RTLview').portMaps;
  const dataWidth = comp.model.ports.find(p => p.name === portMaps.RDATA).wire.width;
  const addrWidth = comp.model.ports.find(p => p.name === portMaps.ARADDR).wire.width;
  const canInterleave = ((e.props || {}).canInterleave || true);
  const maxTransferSize = ((e.props || {}).maxTransferSize || (dataWidth + ' / 8'));

  return `AXI4SlaveNode(Seq(
    AXI4SlavePortParameters(
      slaves = Seq(
        AXI4SlaveParameters(
          address       = List(AddressSet(${params}.base, ((1L << ${addrWidth}) - 1))),
          executable    = ${params}.executable,
          supportsWrite = TransferSizes${portMaps.WDATA ? `(1, (${maxTransferSize}))` : '.none'},
          supportsRead  = TransferSizes${portMaps.RDATA ? `(1, (${maxTransferSize}))` : '.none'},
          interleavedId = ${canInterleave ? 'Some(0)' : 'None'}
        )
      ),
      beatBytes = ${dataWidth} / 8
    )
  ))
`;
};

const masterBusParams = comp => e => {
  const portMaps = e.abstractionTypes.find(e => e.viewRef === 'RTLview').portMaps;

  return `
case class P${e.name}Params(
  executable: Boolean = false,
  maxFifoBits: Int = ${(comp.model.ports.find(p => p.name === portMaps.ARID) || {wire: {width: 2}}).wire.width},
  maxTransactions: Int = 1,
  axi4BufferParams: AXI4BufferParams = AXI4BufferParams(),
  tlBufferParams: TLBufferParams = TLBufferParams()
)
`;
};

const slaveBusParams = comp => e => {
  const portMaps = e.abstractionTypes.find(e => e.viewRef === 'RTLview').portMaps;

  return `
case class P${e.name}Params(
  base: BigInt,
  executable: Boolean = false,
  maxFifoBits: Int = ${(comp.model.ports.find(p => p.name === portMaps.ARID) || {wire: {width: 2}}).wire.width},
  maxTransactions: Int = 1,
  axi4BufferParams: AXI4BufferParams = AXI4BufferParams(),
  tlBufferParams: TLBufferParams = TLBufferParams()
)
`;
};

const slaveNodeGen = comp => e => {
  const params = `c.blackbox.${e.name}Params`;
  const portMaps = e.abstractionTypes.find(e => e.viewRef === 'RTLview').portMaps;
  const dataWidth = comp.model.ports.find(p => p.name === portMaps.RDATA).wire.width;
  const axi4BufferParams = `${params}.axi4BufferParams`;
  const tlBufferParams = `${params}.tlBufferParams`;
  return `
val ${e.name}Node: AXI4SlaveNode = imp.${e.name}Node

def get${e.name}NodeTLAdapter(): TLInwardNode = {(
  ${e.name}Node
    := AXI4Buffer(
      aw = ${axi4BufferParams}.aw,
      ar = ${axi4BufferParams}.ar,
      w = ${axi4BufferParams}.w,
      r = ${axi4BufferParams}.r,
      b = ${axi4BufferParams}.b
    )
    := AXI4UserYanker(capMaxFlight = Some(${params}.maxTransactions))
${e.busType.name === 'AXI4' ?
    `    := AXI4Deinterleaver(c.blackbox.cacheBlockBytes)
         := AXI4IdIndexer(idBits = ${params}.maxFifoBits)` : ''
}    := TLToAXI4()
${e.busType.name === 'AXI4Lite' ?
    `    := TLFragmenter((${dataWidth + ' / 8'}), c.blackbox.cacheBlockBytes, holdFirstDeny=true)` : '\n'
}
    := TLBuffer(
      a = ${tlBufferParams}.a,
      b = ${tlBufferParams}.b,
      c = ${tlBufferParams}.c,
      d = ${tlBufferParams}.d,
      e = ${tlBufferParams}.e
    )
)}
`;
};

const masterNodeGen = comp => e => {
  const params = `c.blackbox.${e.name}Params`;
  const axi4BufferParams = `${params}.axi4BufferParams`;
  const tlBufferParams = `${params}.tlBufferParams`;
  return `
val ${e.name}Node: TLOutwardNode = (
${
  indent(2, ',')(paramSchema.reduce({
    leaf: (node, path) => `val ${path.join('_')}: Int = c.blackbox.${path.join('_')}`
  })(comp.pSchema))
}

  TLBuffer(
    a = ${tlBufferParams}.a,
    b = ${tlBufferParams}.b,
    c = ${tlBufferParams}.c,
    d = ${tlBufferParams}.d,
    e = ${tlBufferParams}.e
  )
    := AXI4ToTL()
    := AXI4UserYanker(capMaxFlight = Some(${params}.maxTransactions))
    := AXI4Fragmenter()
${e.busType.name === 'AXI4' ? `    := AXI4IdIndexer(idBits = ${params}.maxFifoBits)` : ''}
    := AXI4Buffer(
      aw = ${axi4BufferParams}.aw,
      ar = ${axi4BufferParams}.ar,
      w = ${axi4BufferParams}.w,
      r = ${axi4BufferParams}.r,
      b = ${axi4BufferParams}.b
    )
    := imp.${e.name}Node
  )
`;
};

const masterAttachGen = comp => e =>
  `bap.pbus.coupleFrom("axi") { _ := TLWidthWidget(bap.pbus) := ${comp.name}_top.${e.name}Node }`;

const slaveAttachGen = comp => e =>
  `bap.pbus.coupleTo("axi") { ${comp.name}_top.get${e.name}NodeTLAdapter() := TLWidthWidget(bap.pbus) := _ }`;

const busDef = {
  // AW
  AWVALID: 1,
  AWREADY: -1,
  AWID: 'awIdWidth',
  AWADDR: 'awAddrWidth',
  AWLEN: 8,
  AWSIZE: 3,
  AWBURST: 2,
  AWLOCK: 1,
  AWCACHE: 4,
  AWPROT: 3,
  AWQOS: 4,
    // region: 4

  // W
  WVALID: 1,
  WREADY: -1,
  WDATA: 'wDataWidth',
  WSTRB: 'wStrbWidth',
  WLAST: 1,
  // user: 'wUserWidth' // not in IP-XACT

  // B
  BVALID: -1,
  BREADY: 1,
  BID: '-bIdWidth',
  BRESP: -2,
  // user: '-bUserWidth' // not in IP-XACT

  // AR
  ARVALID: 1,
  ARREADY: -1,
  ARID: 'arIdWidth',
  ARADDR: 'addrWidth',
  ARLEN: 8,
  ARSIZE: 3,
  ARBURST: 2,
  ARLOCK: 1,
  ARCACHE: 4,
  ARPROT: 3,
  ARQOS: 4,
  // region: 4
  // user: 'arUserWidth' // not in IP-XACT

  // R
  RVALID: -1,
  RREADY: 1,
  RID: '-rIdWidth',
  RDATA: '-dataWidth',
  RRESP: -2,
  RLAST: -1
  // user: 'rUserWidth' // not in IP-XACT
};

const scalaNames = Object.keys(busDef).reduce((result, name) => {
  if (name.endsWith("VALID")) {
    result[name] = name.slice(0, name.lastIndexOf("VALID")) + ".valid"
  } else if (name.endsWith("READY")) {
    result[name] = name.slice(0, name.lastIndexOf("READY")) + ".ready"
  } else if (name.match(/^[BRW].*/)) {
    result[name] = name[0] + ".bits." + name.slice(1, name.length)
  } else {
    result[name] = name.slice(0, 2) + ".bits." + name.slice(2, name.length)
  }
  result[name] = result[name].toLowerCase()
  return result
}, {});

module.exports = {
  master: {
    adapter: masterAdapterGen,
    params: masterBusParams,
    node: masterNodeGen,
    attach: masterAttachGen,
    wiring: () => () => ''
  },
  slave: {
    adapter: slaveAdapterGen,
    params: slaveBusParams,
    node: slaveNodeGen,
    attach: slaveAttachGen,
    wiring: () => () => ''
  },
  busDef: busDef,
  scalaNames: scalaNames,
  memoryMapped: true
};
