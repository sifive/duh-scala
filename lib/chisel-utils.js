'use strict';

const endent = require('endent');
const traverseSpec = require('./traverse-spec.js');

const convertType = (width) => {
  return (width === 1 ? 'Bool()' : 'UInt((' + width + ').W)');
};

const convertDirection = direction => {
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
  return endent`
    case class ${className}(
      ${params.join(',\n')}
    )
  `;
};

const generateBundle = ({ports, className, paramsMap}) => {
  const nameMap = {};
  const body = ports.map(port => {
    const direction = convertDirection(port.wire.direction);
    const name = port.name;
    const type = convertType(port.wire.width);
    nameMap[port.name] = name;
    return `val ${name} = ${direction}(${type})`;
  });

  const stringParams = Object.entries(paramsMap).map(([key, value]) => {
    return `val ${key}: ${value}`;
  });

  const stringValue = endent`
    class ${className}(
      ${stringParams.join(',\n')}
    ) extends Bundle {
      ${body.join('\n')}
    }
  `;

  return stringValue;
};

const convertParam = ({type, value}) => {
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

const serializeImports = (imports, startPathOpt) => {
  if (!imports) {
    return '';
  }

  var result = [];
  const startPath = startPathOpt || [];
  traverseSpec({
    leaf: (node, path) => {
      const packagePath = startPath.concat(path);
      if (Array.isArray(node)) {
        node.forEach(last => {
          result.push(serializeImports(last, packagePath));
        });
      } else {
        result.push('import ' + packagePath.concat(node).join('.'));
      }
    }
  })(imports);
  return result.join('\n');
};

const generateBlackBox = ({
  className,
  paramsMap,
  blackboxParamsMap,
  ioType,
  ioParamsMap,
  desiredName}) => {

  const stringParams = Object.entries(paramsMap).map(([key, value]) => {
    return `val ${key}: ${value}`;
  });

  const stringBlackBoxParams = Object.entries(blackboxParamsMap).map(([key, value]) => {
    return `"${key}" -> ${value}`;
  });

  const stringIOParams = Object.entries(ioParamsMap).map(([key, value]) => {
    return `${key} = ${value}`;
  });

  const stringValue = endent`
    class ${className}(
      ${stringParams.join(',\n')}
    ) extends BlackBox(Map(
      ${stringBlackBoxParams.join(',\n')}
    )) {
      val io = IO(new ${ioType}(
        ${stringIOParams.join(',\n')}
      ))

      override def desiredName = "${desiredName}"
    }
  `;
  return stringValue;
};

module.exports = {
  generateBlackBox: generateBlackBox,
  generateBundle: generateBundle,
  convertType: convertType,
  convertDirection: convertDirection,
  convertParam: convertParam,
  generateCaseClass: generateCaseClass,
  serializeImports: serializeImports
};
