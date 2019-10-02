'use strict';

const indent = require('./indent.js');
const get = require('lodash.get');

const busDef = {
  CLK: 1,
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
    const readWritePort = `
  when (${io.WREN}) {
    ${e.name}Mem.write(address, writeData, byteEnable)
    ${io.RDDATA} := DontCare
  }.otherwise {
    ${io.RDDATA} := ${e.name}Mem.read(${io.ADDR}, ${io.RDEN}).asUInt
  }`;

    return `
val ${e.name}Node = BundleBridgeSink[${e.name}Bundle]
${e.name}Node := imp.${e.name}Node

InModuleBody {
  val ${e.name}0 = ${e.name}Node.bundle
  val ${e.name}Mem = SyncReadMem(${addrSpace}, Vec(${banks}, UInt((${dataBankWidth}).W)))

  val address = ${io.ADDR}
  val writeData = VecInit(${io.WRDATA}.toBools.grouped(${dataBankWidth}).map(VecInit(_).asUInt).toSeq)
  val byteEnable = ${get(portMaps, ['BEN'], null) ? `${io.BEN}.toBools` : 'VecInit(Seq(true.B))'}

${portMaps.CLK ? `
  val readWriteClock = ${io.CLK}.asClock()
  chisel3.experimental.withClock(readWriteClock) {
${indent(4)(readWritePort)}
  }` :
    indent(2)(readWritePort)
}
}
`;
  }
};

exports.busDef = busDef;
