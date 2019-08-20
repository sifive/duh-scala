'use strict';

const indent = require('./indent');

//// GNERATE BUNDLES ////

const getScalaBundleRegFieldName = (field, idx) => {
  return field.name ? field.name : `reserved${idx}`;
}

const getScalaRegisterBundleName = (register) => {
  return `${register.name}RegisterBundle`;
}

const generateRegisterBundle = register => {
  const fields = register.fields.map((field, idx) => {
    register.fields
    return `val ${getScalaBundleRegFieldName(field, idx)} = UInt(${field.bits}.W)`
  });

  return `class ${getScalaRegisterBundleName(register)} extends Bundle {
${indent(2)(fields.join('\n'))}
}`;
};


const getScalaAddressBlockBundleName = (addressBlock) => {
  return `${addressBlock.name}AddressBlockBundle`;
}

const generateAddressBlockBundles = addressBlock => {
  const registerBundles = addressBlock.registers.map((register) => {
    return generateRegisterBundle(register);
  })

  const bundleFields = addressBlock.registers.map((register) => {
    return `val ${register.name} = new ${getScalaRegisterBundleName(register)}()`;
  })

  return `
${registerBundles.join('\n\n')}

class ${getScalaAddressBlockBundleName(addressBlock)} extends Bundle {
${indent(2)(bundleFields.join('\n'))}
}
`;
};


//// GNERATE REGROUTER ////

const generateRegisterField = field => scalaRegisterName => {
  if (field.access) {
    const regFieldDesc = field.name || field.desc ?
      `, RegFieldDesc(${field.name ? `"${field.name}"` : '""'}, ${field.desc ? `"${field.desc}"` : '""'}))` :
      ')';

    switch (field.access) {
      case 'read-write':
        return `RegField(${field.bits}, ${scalaRegisterName}${regFieldDesc}`;
      case 'read-only':
        return `RegField.r(${field.bits}, ${scalaRegisterName}${regFieldDesc}`;
      case 'write-only':
        return `RegField.w(${field.bits}, ${scalaRegisterName}${regFieldDesc})`;
      case 'read-writeOnce':
        return `RegField(${field.bits}, ${scalaRegisterName}, regFieldWriteOnceFn(${scalaRegisterName})${regFieldDesc}`;
      case 'writeOnce':
        return `RegField(${field.bits}, Unit, regFieldWriteOnceFn(${scalaRegisterName})${regFieldDesc}`;
      default:
        throw new Error(`invalide access field value: ${field.access}`);
    }
  } else {
    return `RegField(${field.bits})`;
  }
};

const generateRegister = register => scalaRegMapBundleName => {
  const scalaRegBundleName = `${scalaRegMapBundleName}.${register.name}`

  const regFieldSeqElements = register.fields.map((field, idx) => {
    const scalaBundleField = getScalaBundleRegFieldName(field, idx)
    return generateRegisterField(field)(`${scalaRegBundleName}.${scalaBundleField}`);
  });

  const registerDesc = register.description ? `Some("${register.description}")` : 'None';

  return `${register.addressOffset} -> RegFieldGroup("${register.name}", ${registerDesc}, Seq(
${indent(2)(regFieldSeqElements.join(',\n'))}))`;
};

const generateBody = addressBlock => packageName => {
  const scalaRegMapBundleName = 'register';

  const registerMapElements = addressBlock.registers.map((register) => {
    return generateRegister(register)(scalaRegMapBundleName);
  });

  const bundleDefs = generateAddressBlockBundles(addressBlock);

  return `${packageName}

import chisel3._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModuleImp
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink.HasTLControlRegMap
import freechips.rocketchip.amba.axi4.HasAXI4ControlRegMap

${bundleDefs}

abstract class ${addressBlock.name}RegRouter(busWidthBytes: Int, baseAddress: Long)(implicit p: Parameters)
  extends IORegisterRouter(
    RegisterRouterParams(
      name = "${addressBlock.name}",
      compat = Seq.empty,
      base = baseAddress,
      beatBytes = busWidthBytes),
    new ${getScalaAddressBlockBundleName(addressBlock)}) {

  private def regFieldWriteOnceFn(register: UInt): RegWriteFn = {
    val written = RegInit(false.B)
    RegWriteFn((valid, data) => {
      when (valid && !written) {
        register := data
        written := true.B
      }
      true.B
    })
  }

  lazy val module = new LazyModuleImp(this) {
    val portValue = port.getWrappedValue
    val ${scalaRegMapBundleName} = RegInit(0.U.asTypeOf(portValue))
    portValue <> ${scalaRegMapBundleName}
    regmap(
${indent(6)(registerMapElements.join(',\n'))})
  }
}

class ${addressBlock.name}TLRegMap(busWidthBytes: Int, baseAddress: Long)(implicit p: Parameters)
  extends ${addressBlock.name}RegRouter(busWidthBytes, baseAddress) with HasTLControlRegMap

class ${addressBlock.name}AXI4RegMap(busWidthBytes: Int, baseAddress: Long)(implicit p: Parameters)
  extends ${addressBlock.name}RegRouter(busWidthBytes, baseAddress) with HasAXI4ControlRegMap
`;
};


module.exports = comp => {
  const memoryMaps = comp.memoryMaps || [];

  let result = {};
  const packageNameBase = `package ${comp.vendor}.${comp.library}.${comp.name}`;
  memoryMaps.forEach((memoryMap) => {
    const addressBlocks = memoryMap.addressBlocks || [];

    let memoryMapResult = {};
    addressBlocks.forEach((addressBlock) => {
      const packageName = `${packageNameBase}.${memoryMap.name}.${addressBlock.name}`;
      memoryMapResult[addressBlock.name] = generateBody(addressBlock)(packageName);
    });

    result[memoryMap.name] = memoryMapResult;
  });

  return result;
};
