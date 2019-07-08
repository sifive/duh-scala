'use strict';

const busDef = {
  // CLK: 1,
  WREN: 1,
  RDEN: 1,
  BEN: 1,
  ADDR: 'addrWidth',
  WRDATA: 'dataWidth',
  RDDATA: '-dataWidth'
  // RDERR: '-errWidth'
};

exports.master = {
  node: comp => e => {
    const portMaps = e.abstractionTypes.find(e => e.viewRef === 'RTLview').portMaps;
    const dataWidth = comp.model.ports.find(p => p.name === portMaps.WRDATA).wire.width;
    const addrWidth = comp.model.ports.find(p => p.name === portMaps.ADDR).wire.width;
    const banks = (comp.model.ports.find(p => p.name === portMaps.BEN) || {wire: {width: 1}}).wire.width;
    const addrSpace = 1 << addrWidth;
    const dataBankWidth = dataWidth / banks;

    const io = Object.keys(busDef).reduce((res, name) => {
      res[name] = e.name + '0.' + name;
      return res;
    }, {});

    return `
val ${e.name}Node = BundleBridgeSink[${e.name}Bundle]
${e.name}Node := imp.${e.name}Node

InModuleBody {
  val ${e.name}0 = ${e.name}Node.bundle
  val ${e.name}Mem = SyncReadMem(${addrSpace}, Vec(${banks}, UInt((${dataBankWidth}).W)))

  val address = ${io.ADDR}
  val writeData = VecInit(${io.WRDATA}.toBools.grouped(${banks}).map(VecInit(_).asUInt).toSeq)
  val byteEnable = ${io.BEN}.toBools

  ${io.RDDATA} := ${e.name}Mem.read(${io.ADDR}, ${io.RDEN})

  when (${io.WREN}) {
    ${e.name}Mem.write(address, writeData, byteEnable)
  }
}
`;
  }
};

exports.busDef = busDef;
