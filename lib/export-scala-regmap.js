'use strict';

const indent = require('./indent');

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
  const direction = (access => {
    switch (access) {
    case 'read-write':
    case 'read-writeOnce':
    case 'write-only':
    case 'writeOnce':
      return 'Output';
    case 'read-only':
      return 'Input';
    case undefined:
      return undefined;
    default:
      throw new Error(`invalid access field value: ${access}`);
    }
  })(access);

  const fields = getRegisterFields(register).map((field, idx) => {
    if (direction) {
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

const generateRegister = (access, register, scalaRegMapBundleName) => {
  const scalaRegBundleName = `${scalaRegMapBundleName}.${register.name}`;

  const regFieldSeqElements = getRegisterFields(register).map((field, idx) => {
    const scalaBundleField = getScalaBundleRegFieldName(field, idx);
    return generateRegisterField(field, `${scalaRegBundleName}.${scalaBundleField}`, access);
  });

  const registerDesc = register.description ? `Some("${register.description}")` : 'None';

  return `${register.addressOffset} -> RegFieldGroup("${register.name}", ${registerDesc}, Seq(
${indent(2)(regFieldSeqElements.join(',\n'))}))`;
};

const getAddressBlockNames = addressBlock => {
  return {
    regRouter: `${addressBlock.name}RegRouter`,
    defaultParamsObject: `${this.regRouter}Defaults`,
    ioBundle: `${addressBlock.name}AddressBlockBundle`,
    regMapRegister: 'register',
    tlRegMap: `${addressBlock.name}TLRegMap`,
    axi4RegMap: `${addressBlock.name}AXI4RegMap`
  };
};

const generateBodyBase = (addressBlock, packageName, compatString) => {
  const names = getAddressBlockNames(addressBlock);

  const registerMapElements = addressBlock.registers.map((register) => {
    return generateRegister(register.access || addressBlock.access, register, names.regMapRegister);
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

object ${names.defaultParamsObject} {
  def name: String = "${addressBlock.name}"
  def compat: Seq[String] = Seq(${compatString})
}

abstract class ${names.regRouter}(busWidthBytes: Int, baseAddress: Long)(implicit p: Parameters)
  extends IORegisterRouter(
    RegisterRouterParams(
      name = ${names.defaultParamsObject}.name,
      compat = ${names.defaultParamsObject}.compat,
      base = baseAddress,
      beatBytes = busWidthBytes),
    new ${names.ioBundle}) {

  lazy val module = new LazyModuleImp(this) {
    val portValue = ioNode.makeIO()
    val ${names.regMapRegister} = RegInit(0.U.asTypeOf(portValue.cloneType.asOutput))
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

const exportRegmap = comp => {
  const memoryMaps = comp.memoryMaps || [];

  let result = {};
  const packageNameBase = `package ${comp.vendor}.${comp.library}.${comp.name}`;
  memoryMaps.forEach((memoryMap) => {
    const addressBlocks = memoryMap.addressBlocks;

    let memoryMapResult = {};
    addressBlocks.forEach((addressBlock) => {
      const packageName = `${packageNameBase}.${memoryMap.name}.${addressBlock.name}`;
      const compatString = `"${comp.vendor},${comp.name}-${comp.version}"`;
      memoryMapResult[addressBlock.name] = generateBodyBase(addressBlock, packageName, compatString);
    });

    result[memoryMap.name] = memoryMapResult;
  });

  return result;
};

module.exports = exportRegmap;
