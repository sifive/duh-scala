'use strict';

const indent = require('./indent.js');
const get = require('lodash.get');

const busDef = {
  WRCLK: 1,
  RDCLK: 1,
  WREN: 1,
  RDEN: 1,
  BEN: 1,
  WRADDR: 'addrWidth',
  WRDATA: 'dataWidth',
  RDADDR: 'addrWidth',
  RDDATA: '-dataWidth'
  // RDERR: '-errWidth'
};

exports.master = {
  node: comp => e => {
    const portMaps = e.abstractionTypes.find(e => e.viewRef === 'RTLview').portMaps;
    const dataWidth = comp.model.ports.find(p => p.name === portMaps.WRDATA).wire.width;
    const addrWidth = comp.model.ports.find(p => p.name === portMaps.WRADDR).wire.width;
    const banks = (comp.model.ports.find(p => p.name === portMaps.BEN) || {wire: {width: 1}}).wire.width;
    const addrSpace = 1 << addrWidth;
    const dataBankWidth = dataWidth / banks;

    const io = Object.keys(busDef).reduce((res, name) => {
      res[name] = e.name + '0.' + name;
      return res;
    }, {});

    const readPort = `${io.RDDATA} := ${e.name}Mem.read(${io.RDADDR}, ${io.RDEN}).asUInt`;
    const writePort = `when (${io.WREN}) {
  ${e.name}Mem.write(writeAddress, writeData, byteEnable)
}`;

    return `
val ${e.name}Node = BundleBridgeSink[${e.name}Bundle]
${e.name}Node := imp.${e.name}Node

InModuleBody {
  val ${e.name}0 = ${e.name}Node.bundle
  val ${e.name}Mem = SyncReadMem(${addrSpace}, Vec(${banks}, UInt((${dataBankWidth}).W)))

  val writeAddress = ${io.WRADDR}
  val writeData = VecInit(${io.WRDATA}.toBools.grouped(${dataBankWidth}).map(VecInit(_).asUInt).toSeq)
  val byteEnable = ${get(portMaps, ['BEN'], null) ? `${io.BEN}.toBools` : 'VecInit(Seq(true.B))'}

${portMaps.RDCLK ? `
  val readClock = ${io.RDCLK}.asClock()
  chisel3.experimental.withClock(readClock) {
${indent(4)(readPort)}
  }` :
    indent(2)(readPort)
}

${portMaps.WRCLK ?`
  val writeClock = ${io.WRCLK}.asClock()
  chisel3.experimental.withClock(writeClock) {
${indent(4)(writePort)}
  }` :
    indent(2)(writePort)
}
}
`;
  }
};

exports.busDef = busDef;
