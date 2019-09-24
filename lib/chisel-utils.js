'use strict';

const indent = require('./indent.js');
const paramSchema = require('./param-schema.js');

const toChiselType = (width) => {
  return (width === 1 ? 'Bool()' : 'UInt((' + width + ').W)');
};

const toChiselDirection = direction => {
  const dirMap = {
    'in': 'Input',
    'out': 'Output',
    'inout': 'Analog'
  };

  const chiselDir = dirMap[direction];

  if (!chiselDir) {
    throw new Error(`${direction} is an invalid chisel direction, must be one of ${Object.keys(dirMap)}`);
  }

  return chiselDir;
};

const generateCaseClass = ({className, fields}) => {
  const params = fields.map((field) => {
    if (field.defaultValue) {
      return `${field.name}: ${field.type} = ${field.defaultValue}`;
    } else {
      return `${field.name}: ${field.type}`;
    }
  });
  return `case class ${className}(
${indent(2, ',')(params)}
)`;
};

const generateChiselBundle = ({ports, className, paramsMap}) => {
  const nameMap = {};
  const body = ports.map(port => {
    const direction = toChiselDirection(port.wire.direction);
    const name = port.name;
    const type = toChiselType(port.wire.width);
    nameMap[port.name] = name;
    return `val ${name} = ${direction}(${type})`;
  });

  const stringParams = Object.entries(paramsMap).map(([key, value]) => {
    return `val ${key}: ${value}`;
  });

  const stringValue = `class ${className}(
${indent(2, ',')(stringParams)}
) extends Bundle {
${indent(2)(body)}
}`;

  return stringValue;
};

const toChiselParam = ({type, value}) => {
  const typeMap = {
    'int': 'IntParam',
    'double': 'DoubleParam',
    'string': 'StringParam',
    'raw': 'RawParam'
  };

  const chiselType = typeMap[type];

  if (chiselType) {
    return `${chiselType}(${value})`;
  } else {
    throw new Error(`${type} is an invalid chisel blackbox parameter type, must be one of ${Object.keys(typeMap)}`);
  }
};

const generateBlackBoxModule = ({className, paramsMap, blackboxParamsMap, ioType, ioParamsMap, desiredName}) => {

  const stringParams = Object.entries(paramsMap).map(([key, value]) => {
    return `val ${key}: ${value}`;
  });

  const stringBlackBoxParams = Object.entries(blackboxParamsMap).map(([key, value]) => {
    return `"${key}" -> ${value}`;
  });

  const stringIOParams = Object.entries(ioParamsMap).map(([key, value]) => {
    return `${key} = ${value}`;
  });
  const stringValue = `class ${className}(
${indent(2, ',')(stringParams)}
) extends BlackBox(Map(
${indent(2, ',')(stringBlackBoxParams)}
)) {
  val io = IO(new ${ioType}(
${indent(4, ',')(stringIOParams)}
  ))

  override val desiredName = "${desiredName}"
}`;

  return stringValue;
};

module.exports = {
  generateBlackBox: generateBlackBoxModule,
  generateBundle: generateChiselBundle,
  convertType: toChiselType,
  convertDirection: toChiselDirection,
  convertParam: toChiselParam,
  generateCaseClass: generateCaseClass
};
