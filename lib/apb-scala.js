'use strict';

const masterAdapterGen = comp => () => `APBMasterNode(Seq(
    APBMasterPortParameters(
      masters = Seq(APBMasterParameters(
        name = "${comp.name}"
        // nodePath
      )))))
  `;

const slaveAdapterGen = comp => e => {
  const params = `c.${e.name}Params`;
  const portMaps = e.abstractionTypes.find(e => e.viewRef === 'RTLview').portMaps;
  const dataWidth = comp.model.ports.find(p => p.name === portMaps.PRDATA).wire.width;
  const addrWidth = comp.model.ports.find(p => p.name === portMaps.PADDR).wire.width;
  return `APBSlaveNode(Seq(
    APBSlavePortParameters(
      slaves = Seq(APBSlaveParameters(
        address = List(AddressSet(${params}.base, 0x${(Math.pow(2, addrWidth) - 1).toString(16)}L)),
        // resources
        // regionType
        executable = false,
        // nodePath
        supportsWrite = true,
        supportsRead  = true
        // device
      )),
      beatBytes = ${dataWidth} / 8
    )
  ))
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
  const portMaps = e.abstractionTypes.find(e => e.viewRef === 'RTLview').portMaps;
  const dataWidth = comp.model.ports.find(p => p.name === portMaps.PRDATA).wire.width;
  return `
val ${e.name}Node: APBSlaveNode = imp.${e.name}Node

def get${e.name}NodeTLAdapter(): TLInwardNode = {(
  ${e.name}Node
    := TLToAPB(false)
    := TLBuffer()
    := TLFragmenter((${dataWidth + ' / 8'}), c.blackbox.cacheBlockBytes, holdFirstDeny=true)
)}
`;
};

const masterNodeGen = () => e => `val ${e.name}Node: TLOutwardNode = (
  APBToTL()
    := imp.${e.name}Node
)
`;

const busDef = {
  PRDATA: 'dataWidth',
  PWRITE: 1,
  PENABLE: 1,
  PSELx: 1,
  PREADY: -1,
  PSLVERR: 1,
  PADDR: 'addrWidth',
  PWDATA: 'dataWidth',
  PPROT: 3
};

const scalaNames = {
  PRDATA:'prdata',
  PWRITE: 'pwrite',
  PENABLE: 'penable',
  PSELx: 'psel',
  PREADY: 'pready',
  PSLVERR: 'pslverr',
  PADDR: 'paddr',
  PWDATA: 'pwdata',
  PPROT: 'pprot'
};

const slaveAttachGen = comp => e =>
  `bap.pbus.coupleTo("${comp.name}_apb") { ${comp.name}_top.get${e.name}NodeTLAdapter() := TLWidthWidget(bap.pbus) := _ }`;

const masterAttachGen = comp => e =>
  `bap.pbus.coupleFrom("axi") { _ := TLWidthWidget(bap.pbus) := ${comp.name}_top.${e.name}Node }`;

module.exports = {
  master: {
    adapter: masterAdapterGen,
    // params: masterBusParams,
    node: masterNodeGen,
    attach: masterAttachGen
    // wiring: () => () => ''
  },
  slave: {
    adapter: slaveAdapterGen,
    params: slaveBusParams,
    node: slaveNodeGen,
    attach: slaveAttachGen
    // wiring: () => () => ''
  },
  busDef: busDef,
  scalaNames: scalaNames,
  memoryMapped: true
};
