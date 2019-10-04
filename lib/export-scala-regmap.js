'use strict';

const indent = require('./indent.js');

//// GNERATE BUNDLES ////

const getScalaBundleRegFieldName = (field, idx) => {
  return field.name ? field.name : `reserved${idx}`;
};

const getScalaRegisterBundleName = (register) => {
  return `${register.name}RegisterBundle`;
};

const getRegisterFields = register => {
  return register.fields || [{bits: register.size}];
};

const generateRegisterBundle = (access, register) => {
  const fields = getRegisterFields(register).map((field, idx) => {
    const fieldAccess = field.access || access;
    if (fieldAccess) {
      const direction = (access => {
        switch (access) {
        case 'read-write':
        case 'read-writeOnce':
        case 'write-only':
        case 'writeOnce':
          return 'Output';
        case 'read-only':
          return 'Input';
        default:
          throw new Error(`invalid access field value: ${access}`);
        }
      })(fieldAccess);
      return `val ${getScalaBundleRegFieldName(field, idx)} = ${direction}(UInt(${field.bits}.W))`;
    } else {
      return `// PAD ${field.bits}`;
    }
  });

  return `class ${getScalaRegisterBundleName(register)} extends Bundle {
${indent(2)(fields.join('\n'))}
}`;
};

const generateAddressBlockBundles = addressBlock => {
  const registerBundles = addressBlock.registers.map((register) => {
    return generateRegisterBundle(register.access || addressBlock.access, register);
  });

  const bundleFields = addressBlock.registers.map((register) => {
    return `val ${register.name} = new ${getScalaRegisterBundleName(register)}()`;
  });

  const names = getAddressBlockNames(addressBlock);

  return `${registerBundles.join('\n\n')}

class ${names.ioBundle} extends Bundle {
${indent(2)(bundleFields.join('\n'))}
}`;
};


//// GNERATE REGROUTER ////

const generateRegisterField = (field, scalaRegisterName, access) => {
  if (access) {
    const regFieldDesc = field.name || field.desc ?
      `RegFieldDesc(${field.name ? `"${field.name}"` : '""'}, ${field.desc ? `"${field.desc}"` : '""'})` :
      '';

    switch (access) {
    case 'read-write':
    case 'read-writeOnce':
      return `RegField(${field.bits}, ${scalaRegisterName}, ${regFieldDesc})`;
    case 'read-only':
      return `RegField.r(${field.bits}, ${scalaRegisterName}, ${regFieldDesc})`;
    case 'write-only':
    case 'writeOnce':
      return `RegField.w(${field.bits}, ${scalaRegisterName}, ${regFieldDesc})`;
    default:
      throw new Error(`invalid access field value: ${access}`);
    }
  } else {
    return `RegField(${field.bits})`;
  }
};

const generateRegister = (access, register, addressBlockNames) => {
  const scalaRegBundleName = `${addressBlockNames.regMapResetWire}.${register.name}`;

  const regFieldSeqElements = getRegisterFields(register).map((field, idx) => {
    const scalaBundleField = getScalaBundleRegFieldName(field, idx);
    return generateRegisterField(field, `${scalaRegBundleName}.${scalaBundleField}`, field.access || access);
  });

  const registerDesc = register.description ? `Some("${register.description}")` : 'None';

  return `${register.addressOffset} -> RegFieldGroup("${register.name}", ${registerDesc}, Seq(
${indent(2)(regFieldSeqElements.join(',\n'))}))`;
};

const generateRegisterResetParams = (register, addressBlockNames) => {
  const baseParamsName =
    `${addressBlockNames.baseParamsObject.name}.${addressBlockNames.baseParamsObject.resetValues}.${register.name}`;

  const registerResetValues = getRegisterFields(register).map((field, idx) => {
    const paramName = getScalaBundleRegFieldName(field, idx);
    return `def ${paramName}: UInt =
  ${baseParamsName}.${paramName}`;
  });

  const objectName = register.name;
  return `object ${objectName} {
${indent(2)(registerResetValues.join('\n'))}
}`;
};

const generateRegisterResetBaseParams = (register) => {
  const registerResetValues = getRegisterFields(register).map((field, idx) => {
    const resetValue = field.resetValue ? `${field.resetValue}.U` : '0.U';
    const paramName = getScalaBundleRegFieldName(field, idx);
    return `def ${paramName}: UInt = ${resetValue}`;
  });

  const objectName = register.name;
  return `object ${objectName} {
${indent(2)(registerResetValues.join('\n'))}
}`;
};

const generateRegisterReset = (access, register, addressBlockNames) => {
  const scalaRegBundleName = `${addressBlockNames.regMapResetWire}.${register.name}`;
  const resetBundleName =
    `${addressBlockNames.userParamsObject.name}.${addressBlockNames.userParamsObject.resetValues}.${register.name}`;

  const regFieldSeqElements = getRegisterFields(register).map((field, idx) => {
    const scalaBundleField = getScalaBundleRegFieldName(field, idx);
    const resetValue = `${resetBundleName}.${scalaBundleField}`;
    const resetWire = `${scalaRegBundleName}.${scalaBundleField}`;
    return access ? `${resetWire} := ${resetValue}` : `RegField(${field.bits})`;
  });

  return regFieldSeqElements.join('\n');
};

const getAddressBlockNames = addressBlock => {
  const regRouterName = `${addressBlock.name}RegRouter`;
  return {
    regRouter: regRouterName,
    baseParamsObject: {
      name: `${regRouterName}Base`,
      resetValues: 'resetValues',
      deviceTreeName: 'deviceTreeName',
      deviceTreeCompat: 'deviceTreeCompat'
    },
    userParamsObject: {
      name: `${regRouterName}User`,
      resetValues: 'resetValues',
      deviceTreeName: 'deviceTreeName',
      deviceTreeCompat: 'deviceTreeCompat'
    },
    ioBundle: `${addressBlock.name}AddressBlockBundle`,
    regMapRegister: 'register',
    regMapResetWire: 'resetValue',
    tlRegMap: `${addressBlock.name}TLRegMap`,
    axi4RegMap: `${addressBlock.name}AXI4RegMap`
  };
};

const generateBodyBase = (names, addressBlock, packageName, deviceNameString, compatString) => {
  const registerMapElements = addressBlock.registers.map((register) => {
    return generateRegister(register.access || addressBlock.access, register, names);
  });

  const resetElements = addressBlock.registers.map((register) => {
    return generateRegisterReset(register.access || addressBlock.access, register, names);
  });

  const resetParamElements = addressBlock.registers.map((register) => {
    return generateRegisterResetBaseParams(register);
  });

  const bundleDefs = generateAddressBlockBundles(addressBlock);

  return `// Generated Code
// Please DO NOT EDIT


${packageName}

import chisel3._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModuleImp
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink.HasTLControlRegMap
import freechips.rocketchip.amba.axi4.HasAXI4ControlRegMap

${bundleDefs}

object ${names.baseParamsObject.name} {
  def ${names.baseParamsObject.deviceTreeName}: String = ${deviceNameString}
  def ${names.baseParamsObject.deviceTreeCompat}: Seq[String] = Seq(${compatString})

  object ${names.baseParamsObject.resetValues} {
${indent(4)(resetParamElements.join('\n\n'))}
  }
}

abstract class ${names.regRouter}(busWidthBytes: Int, baseAddress: Long)(implicit p: Parameters)
  extends IORegisterRouter(
    RegisterRouterParams(
      name = ${names.userParamsObject.name}.${names.userParamsObject.deviceTreeName},
      compat = ${names.userParamsObject.name}.${names.userParamsObject.deviceTreeCompat},
      base = baseAddress,
      beatBytes = busWidthBytes),
    new ${names.ioBundle}) {

  lazy val module = new LazyModuleImp(this) {
    val portValue = ioNode.makeIO()
    val ${names.regMapResetWire} = Wire(portValue.cloneType.asOutput)
${indent(4)(resetElements.join('\n'))}

    val ${names.regMapRegister} = RegInit(resetValue)
    portValue <> ${names.regMapRegister}

    val mapping = Seq(
${indent(6)(registerMapElements.join(',\n'))})
    regmap(mapping:_*)
  }
}

class ${names.tlRegMap}(busWidthBytes: Int, baseAddress: Long)(implicit p: Parameters)
  extends ${names.regRouter}(busWidthBytes, baseAddress) with HasTLControlRegMap

class ${names.axi4RegMap}AXI4RegMap(busWidthBytes: Int, baseAddress: Long)(implicit p: Parameters)
  extends ${names.regRouter}(busWidthBytes, baseAddress) with HasAXI4ControlRegMap
`;
};

const generateBodyUser = (names, addressBlock, packageName) => {
  const resetParamElements = addressBlock.registers.map((register) => {
    return generateRegisterResetParams(register, names);
  });

  return `${packageName}

import chisel3._

object ${names.userParamsObject.name} {
  def ${names.userParamsObject.deviceTreeName}: String = ${names.baseParamsObject.name}.${names.baseParamsObject.deviceTreeName}
  def ${names.userParamsObject.deviceTreeCompat}: Seq[String] = ${names.baseParamsObject.name}.${names.baseParamsObject.deviceTreeCompat}

  object ${names.userParamsObject.resetValues} {
${indent(4)(resetParamElements.join('\n\n'))}
  }
}
`;
};

const exportRegmap = comp => {
  const memoryMaps = comp.memoryMaps || [];

  let baseResult = {};
  let userResult = {};
  const packageNameBase = `package ${comp.vendor}.${comp.library}.${comp.name}`;
  memoryMaps.forEach((memoryMap) => {
    const addressBlocks = memoryMap.addressBlocks;

    let memoryMapUserResult = {};
    let memoryMapBaseResult = {};
    addressBlocks.forEach((addressBlock) => {
      const packageName = `${packageNameBase}.${memoryMap.name}.${addressBlock.name}`;
      const compatString = `"${comp.vendor},${comp.name}-${comp.version}"`;
      const deviceNameString = `"${comp.name}-${addressBlock.name}"`;
      const names = getAddressBlockNames(addressBlock);

      memoryMapBaseResult[`${addressBlock.name}-base`] = generateBodyBase(names, addressBlock, packageName, deviceNameString, compatString);
      memoryMapUserResult[addressBlock.name] = generateBodyUser(names, addressBlock, packageName);
    });

    baseResult[memoryMap.name] = memoryMapBaseResult;
    userResult[memoryMap.name] = memoryMapUserResult;
  });

  return {
    base: baseResult,
    user: userResult
  };
};

const generateOMRegisterMap = (addressBlock) => {
  const registerMapElements = addressBlock.registers.map((register) => {
    const regFieldSeqElements = (register.fields || []).map(field => {
      const access = field.access || register.access || addressBlock.access;
      return generateRegisterField(field, 'Bool()', access);
    });

    const registerDesc = register.description ? `Some("${register.description}")` : 'None';

    return `${register.addressOffset} -> RegFieldGroup("${register.name}", ${registerDesc}, Seq(
${indent(2, ',')(regFieldSeqElements)}))`;
  });

  return `OMRegister.convert(
${indent(2)(registerMapElements.join(',\n'))})`;
};

module.exports = {
  exportRegmap: exportRegmap,
  generateOMRegisterMap: generateOMRegisterMap
};
