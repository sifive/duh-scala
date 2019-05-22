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
    const rdenWidth = comp.model.ports.find(p => p.name === portMaps.RDEN).wire.width;
    const addrSpace = 1 << addrWidth;
    const banks = rdenWidth;
    const dataBankWidth = dataWidth / banks;

    const io = Object.keys(busDef).reduce((res, name) => {
      res[name] = 'imp.module.' + e.name + '0.' + name;
      return res;
    }, {});

    return `
InModuleBody {
  val ${e.name}Mem = SyncReadMem(${addrSpace}L, Vec(${banks}, UInt((${dataBankWidth}).W)))
  // withClockAndReset(clock, reset) {}
  ${io.RDDATA} := ${e.name}Mem.read(${io.ADDR}, ${io.RDEN})
  when (${io.WREN}) {
    ${e.name}Mem(${io.ADDR}) := ${io.WRDATA}
  }
}
`;
  }
};

exports.busDef = busDef;
