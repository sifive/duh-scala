'use strict';

const indent = require('./indent.js');
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
  const maxBurst = (e.busType.name === 'AXI4-Lite') ? 1 : ((e.props || {}).maxBurst || 16);
  const maxTransferSize = ((dataWidth + ' * ' + maxBurst + ' / 8'));
  return `AXI4SlaveNode(Seq(
    AXI4SlavePortParameters(
      slaves = Seq(
        AXI4SlaveParameters(
          address       = List(AddressSet(${params}.base, ((1L << ${addrWidth}) - 1))),
          executable    = ${params}.executable,
          supportsWrite = TransferSizes${portMaps.WDATA ? `(1, (${maxTransferSize}))` : '.none'},
          supportsRead  = TransferSizes${portMaps.RDATA ? `(1, (${maxTransferSize}))` : '.none'},
          interleavedId = ${canInterleave ? 'Some(0)' : 'None'},
          resources     = device.reg("${e.memoryMapRef}")
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
${e.busType.name === 'AXI4-Lite' ?
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
  := TLFIFOFixer()
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

// fields defined in bus spec but not in scala bundle, values are tie-off values
const excludes = {
  aw: {
    bits: {
      region: 0
    }
  },
  ar: {
    bits: {
      region: 0
    }
  }
};

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
      qos: 4,
      region: 4
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
      qos: 4,
      region: 4
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
  busDef: busDef,
  memoryMapped: true,
  monitor: {
    names: {
      args: 'AXI4MonitorArgs',
      baseMonitor: 'AXI4MonitorBase',
      edgeParams: 'AXI4EdgeParameters',
      bundle: 'AXI4Bundle'
    },
    imports: [
      'freechips.rocketchip.amba.axi4.{AXI4Bundle, AXI4EdgeParameters, AXI4MonitorArgs, AXI4MonitorBase}'
    ]
  },
  excludes: excludes
};
