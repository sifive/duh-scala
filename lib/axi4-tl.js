'use strict';

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
  const addrWidth = Math.max(
    comp.model.ports.find(p => p.name === portMaps.ARADDR).wire.width,
    comp.model.ports.find(p => p.name === portMaps.AWADDR).wire.width
  );
  const canInterleave = ((e.props || {}).canInterleave || true);
  const maxTransferSize = ((e.props || {}).maxTransferSize || (dataWidth / 8));

  return `AXI4SlaveNode(Seq(
    AXI4SlavePortParameters(
      slaves = Seq(
        AXI4SlaveParameters(
          address       = List(AddressSet(c.${e.name}_base, ${Math.pow(2, addrWidth).toString(16)}L - 1)),
          executable    = ${params}.executable,
          supportsWrite = TransferSizes${portMaps.WDATA ? `(1, ${maxTransferSize})` : '.none'},
          supportsRead  = TransferSizes${portMaps.RDATA ? `(1, ${maxTransferSize})` : '.none'},
          interleavedId = ${canInterleave ? 'Some(0)' : 'None'}
        )
      ),
      beatBytes = ${dataWidth / 8}
    )
  ))
`;
};

const masterBusParams = comp => e => {
  const portMaps = e.abstractionTypes.find(e => e.viewRef === 'RTLview').portMaps;

  return `
case class P${e.name}Params(
  executable: Bool = false.B,
  burstBytes: Int,
  maxBurst: Option[Int] = None, // use cache block // NEEDED IF R-interleaving is allowed
  maxFifoBits: Int = ${(comp.model.ports.find(p => p.name === portMaps.ARID) || {wire: {width: 2}}).wire.width},
  maxTransactions: Int = 1,
  axi4BufferParams: AXI4BufferParams = AXI4BufferParams(),
  tlBufferParams: TLBufferParams = TLBufferParams()
)
`;
};

const slaveBusParams = masterBusParams;

const slaveNodeGen = comp => e => {
  const params = `c.${e.name}Params`;
  const portMaps = e.abstractionTypes.find(e => e.viewRef === 'RTLview').portMaps;
  const dataWidth = comp.model.ports.find(p => p.name === portMaps.RDATA).wire.width;
  const axi4BufferParams = `${params}.axi4BufferParams`;
  const tlBufferParams = `${params}.tlBufferParams`;
  return `
val ${e.name}Node: TLInwardNode = (
  imp.${e.name}Node
    := AXI4Buffer(
      aw = ${axi4BufferParams}.aw,
      ar = ${axi4BufferParams}.ar,
      w = ${axi4BufferParams}.w,
      r = ${axi4BufferParams}.r,
      b = ${axi4BufferParams}.b
    )
    := AXI4UserYanker(capMaxFlight = Some(${params}.maxTransactions))
${e.busType.name === 'AXI4' ? `
    := AXI4Deinterleaver(${params}.maxBurst)
    := AXI4IdIndexer(idBits = ${params}.maxFifoBits)` : ''}
    := TLToAXI4()
    ${e.busType.name === 'AXI4Lite' ? `
    := TLFragmenter(${dataWidth / 8}, ${params}.burstBytes, holdFirstDeny=true)` : ''}
    := TLBuffer(
      a = ${tlBufferParams}.a,
      b = ${tlBufferParams}.b,
      c = ${tlBufferParams}.c,
      d = ${tlBufferParams}.d,
      e = ${tlBufferParams}.e
    )
)
`;
};

const masterNodeGen = () => e => {
  const params = `c.${e.name}Params`;
  const axi4BufferParams = `${params}.axi4BufferParams`;
  const tlBufferParams = `${params}.tlBufferParams`;
  return `
val ${e.name}Node: TLOutwardNode = (
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
  `bap.pbus.coupleTo("axi") { ${comp.name}_top.${e.name}Node := TLWidthWidget(bap.pbus) := _ }`;

const slaveAttachGen = masterAttachGen;

const busDef = {
  aw: {
    valid: 1,
    ready: -1,
    bits: {
      id: 'awIdWidth',
      addr: 'awAddrWidth',
      len: 8,
      size: 3,
      burst: 2,
      lock: 1,
      cache: 4,
      prot: 3,
      qos: 4
      // region: 4
    }
  },
  w: {
    valid: 1,
    ready: -1,
    bits: {
      data: 'wDataWidth',
      strb: 'wStrbWidth',
      last: 1
      // user: 'wUserWidth' // not in IP-XACT
    }
  },
  b: {
    valid: -1,
    ready: 1,
    bits: {
      id: '-bIdWidth',
      resp: -2
      // user: '-bUserWidth' // not in IP-XACT
    }
  },
  ar: {
    valid: 1,
    ready: -1,
    bits: {
      id: 'arIdWidth',
      addr: 'addrWidth',
      len: 8,
      size: 3,
      burst: 2,
      lock: 1,
      cache: 4,
      prot: 3,
      qos: 4
      // region: 4
      // user: 'arUserWidth' // not in IP-XACT
    }
  },
  r: {
    valid: -1,
    ready: 1,
    bits: {
      id: '-rIdWidth',
      data: '-dataWidth',
      resp: -2,
      last: -1
      // user: 'rUserWidth' // not in IP-XACT
    }
  }
};

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
  busDef: busDef
};
