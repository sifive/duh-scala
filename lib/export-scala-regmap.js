'use strict';

const chisel = require('./chisel-utils.js');
const endent = require('endent');
const paramSchema = require('./param-schema.js');

// Check whether value is defined, counting 0 as truthy.
const isDefinedNumber = i => !!(i || i === 0);

//// GNERATE BUNDLES ////

const getScalaBundleRegFieldName = (field, idx) => {
  return field.name ? field.name : `reserved${idx}`;
};

const getScalaRegisterBundleName = (register) => {
  return `${register.name}RegisterBundle`;
};

const getRegisterFields = register => {
  return register.fields || [{bitOffset: 0, bitWidth: register.size}];
};

const accessToDirection = access => {
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
};

const generateRegisterBundle = (access, register, blackboxParams) => {
  const fields = getRegisterFields(register).map((field, idx) => {
    const fieldAccess = field.access || access;
    if (fieldAccess) {
      const direction = (access => {
        return accessToDirection(access);
      })(fieldAccess);
      return `val ${getScalaBundleRegFieldName(field, idx)} = ${direction}(UInt(${field.bitWidth}.W))`;
    } else {
      return `// PAD ${field.bitWidth}`;
    }
  });

  //TODO: only Int params supported
  const params = blackboxParams.fields.map(param => {
    return `val ${param.name}: Int`;
  });

  return endent`
  class ${getScalaRegisterBundleName(register)}(
    ${params.join(',\n')}
  ) extends Bundle {
    ${fields.join('\n')}
  }`;
};

const generateAddressBlockBundles = (addressBlock, blackboxParams) => {
  const registerBundles = addressBlock.registers.map((register) => {
    return generateRegisterBundle(register.access || addressBlock.access, register, blackboxParams);
  });

  const bundleFields = addressBlock.registers.map((register) => {
    const params = blackboxParams.fields.map(field => {
      return `${field.name} = params.${field.name}`;
    });

    return endent`
      val ${register.name} = new ${getScalaRegisterBundleName(register)}(
        ${params.join(',\n')}
      )`;
  });

  const names = getAddressBlockNames(addressBlock);

  return registerBundles.concat(endent`
    class ${names.ioBundle}(val params: ${blackboxParams.className}) extends Bundle {
      ${bundleFields.join('\n')}
    }`);
};


//// GNERATE REGROUTER ////

const generateRegisterField = (field, scalaRegisterName, access, resetValue) => {
  if (access) {
    var result = '';
    switch (access) {
    case 'read-write':
    case 'read-writeOnce':
      result += 'RegField';
      break;
    case 'read-only':
      result += 'RegField.r';
      break;
    case 'write-only':
    case 'writeOnce':
      result += 'RegField.w';
      break;
    default:
      throw new Error(`invalid access field value: ${access}`);
    }
    result += `(${field.bitWidth}, ${scalaRegisterName}`;
    if (field.name || field.desc || isDefinedNumber(resetValue)) {
      const resetValueString = isDefinedNumber(resetValue) ? `, reset = Some(${resetValue})`: '';
      result += `, RegFieldDesc("${field.name || ''}", "${field.desc || ''}"${resetValueString})`;
    }
    result += ')';
    return result;
  } else {
    return `RegField(${field.bitWidth})`;
  }
};

const generateRegister = (addressUnitBits, access, register, addressBlockNames) => {
  const scalaRegBundleName = `${addressBlockNames.regMapRegister}.${register.name}`;

  const regFieldSeqElements = getRegisterFields(register).map((field, idx) => {
    const scalaBundleField = getScalaBundleRegFieldName(field, idx);
    return `${field.bitOffset} -> ` + generateRegisterField(field, `${scalaRegBundleName}.${scalaBundleField}`, field.access || access);
  });

  const registerDesc = register.description ? `Some("${register.description}")` : 'None';

  const addressOffset =
    addressUnitBits === 8 ? register.addressOffset : `${register.addressOffset} * ${register.addressSizeUnit}`;

  return endent`
    ${addressOffset} -> RegFieldGroup("${register.name}", ${registerDesc}, padFields(
      ${regFieldSeqElements.join(',\n')}))`;
};

const generateRegisterResetParams = (register, addressBlockNames) => {
  const baseParamsName =
    `${addressBlockNames.baseParamsObject.name}.${addressBlockNames.baseParamsObject.resetValues}.${register.name}`;

  const registerResetValues = getRegisterFields(register).map((field, idx) => {
    const paramName = getScalaBundleRegFieldName(field, idx);
    return endent`
      def ${paramName}: UInt =
        ${baseParamsName}.${paramName}`;
  });

  const objectName = register.name;
  return endent`
    object ${objectName} {
      ${registerResetValues.join('\n')}
    }`;
};

const generateRegisterResetBaseParams = (register) => {
  const registerResetValues = getRegisterFields(register).map((field, idx) => {
    const resetValue = field.resetValue ? `${field.resetValue}.U` : '0.U';
    const paramName = getScalaBundleRegFieldName(field, idx);
    return `def ${paramName}: UInt = ${resetValue}`;
  });

  const objectName = register.name;
  return endent`
    object ${objectName} {
      ${registerResetValues.join('\n')}
    }`;
};

const generateRegisterReset = (parentAccess, register, addressBlockNames) => {
  const scalaRegBundleName = `${addressBlockNames.regMapResetWire}.${register.name}`;
  const resetBundleName =
    `${addressBlockNames.userParamsObject.name}.${addressBlockNames.userParamsObject.resetValues}.${register.name}`;

  const regFieldSeqElements = getRegisterFields(register).map((field, idx) => {
    const scalaBundleField = getScalaBundleRegFieldName(field, idx);
    const resetValue = `${resetBundleName}.${scalaBundleField}`;
    const resetWire = `${scalaRegBundleName}.${scalaBundleField}`;
    const access = field.access || parentAccess;
    return access ? `${resetWire} := ${resetValue}` : '';
  });

  return regFieldSeqElements.join('\n');
};

const generatePortConnections = (parentAccess, register, addressBlockNames) => {
  const scalaRegisterName = `${addressBlockNames.regMapRegister}.${register.name}`;
  const scalaPortName = `${addressBlockNames.regMapPort}.${register.name}`;

  const regFieldSeqElements = getRegisterFields(register).map((field, idx) => {
    const scalaBundleField = getScalaBundleRegFieldName(field, idx);
    const port = `${scalaPortName}.${scalaBundleField}`;
    const register = `${scalaRegisterName}.${scalaBundleField}`;
    const access = field.access || parentAccess;
    return accessToDirection(access) === 'Output' ? `${port} := ${register}` : `${register} := ${port}`;
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
    regMapPort: 'port',
    paramsName: 'componentParams',
    tlRegMap: `${addressBlock.name}TLRegMap`,
    axi4RegMap: `${addressBlock.name}AXI4RegMap`
  };
};

const padFieldsMethod = endent`
  private def padFields(fields: (Int, RegField)*) = {
    var previousOffset = 0
    var previousField: Option[RegField] = None

    fields.flatMap { case (fieldOffset, field) =>
      val padWidth = fieldOffset - (previousOffset + previousField.map(_.width).getOrElse(0))
      require(padWidth >= 0,
        if (previousField.isDefined) {
          s"register fields at $previousOffset and $fieldOffset are overlapping"
        } else {
          s"register field $field has a negative offset"
        })

      previousOffset = fieldOffset
      previousField = Some(field)

      if (padWidth > 0) {
        Seq(RegField(padWidth), field)
      } else {
        Seq(field)
      }
    }
  }`;

const generateBodyBase = ({
  names,
  addressUnitBits,
  addressBlock,
  deviceNameString,
  compatString,
  blackboxParams
}) => {
  const registerMapElements = addressBlock.registers.map((register) => {
    return generateRegister(
      addressUnitBits,
      register.access || addressBlock.access,
      register,
      names);
  });

  const resetElements = addressBlock.registers.map((register) => {
    return generateRegisterReset(register.access || addressBlock.access, register, names);
  });

  const portElements = addressBlock.registers.map((register) => {
    return generatePortConnections(register.access || addressBlock.access, register, names);
  });

  const resetParamElements = addressBlock.registers.map((register) => {
    return generateRegisterResetBaseParams(register);
  });

  const bundleDefs = generateAddressBlockBundles(addressBlock, blackboxParams);
  const baseParamsObject = endent`
    object ${names.baseParamsObject.name} {
      def ${names.baseParamsObject.deviceTreeName}: String = ${deviceNameString}
      def ${names.baseParamsObject.deviceTreeCompat}: Seq[String] = Seq(${compatString})

      object ${names.baseParamsObject.resetValues} {
        ${resetParamElements.join('\n\n')}
      }
    }
  `;

  const blackboxParamAliases = blackboxParams.fields.map(field => {
    return `val ${field.name} = ${names.paramsName}.${field.name}`;
  });

  const registerRouter = endent`
    abstract class ${names.regRouter}(
      busWidthBytes: Int,
      baseAddress: Long,
      ${names.paramsName}: ${blackboxParams.className})(implicit p: Parameters)
      extends IORegisterRouter(
        RegisterRouterParams(
          name = ${names.userParamsObject.name}.${names.userParamsObject.deviceTreeName},
          compat = ${names.userParamsObject.name}.${names.userParamsObject.deviceTreeCompat},
          base = baseAddress,
          beatBytes = busWidthBytes),
        new ${names.ioBundle}(${names.paramsName})) {

      ${padFieldsMethod}

      lazy val module = new LazyModuleImp(this) {
        val ${names.regMapResetWire} = Wire(port.cloneType.asOutput)
        ${resetElements.join('\n')}

        val ${names.regMapRegister} = RegInit(resetValue)
        ${portElements.join('\n')}

        val mapping = {
          ${blackboxParamAliases.join('\n')}
          Seq(
            ${registerMapElements.join(',\n')})
        }

        regmap(mapping:_*)
      }
    }
  `;

  const tlRegisterRouter = endent`
    class ${names.tlRegMap}(busWidthBytes: Int, baseAddress: Long, ${names.paramsName}: ${blackboxParams.className})(implicit p: Parameters)
      extends ${names.regRouter}(busWidthBytes, baseAddress, ${names.paramsName}) with HasTLControlRegMap`;

  const axi4RegisterRouter = endent`
    class ${names.axi4RegMap}AXI4RegMap(busWidthBytes: Int, baseAddress: Long, ${names.paramsName}: ${blackboxParams.className})(implicit p: Parameters)
      extends ${names.regRouter}(busWidthBytes, baseAddress, ${names.paramsName}) with HasAXI4ControlRegMap`;

  return {
    imports: [
      'chisel3._',
      '',
      {
        'freechips.rocketchip': [
          'amba.axi4.HasAXI4ControlRegMap',
          'config.Parameters',
          'diplomacy.LazyModuleImp',
          'regmapper._',
          'tilelink.HasTLControlRegMap'
        ]
      },
      '',
      blackboxParams.packageName + '.' + blackboxParams.className
    ],
    blocks: bundleDefs.concat([
      baseParamsObject,
      registerRouter,
      tlRegisterRouter,
      axi4RegisterRouter
    ])
  };
};

const generateBodyUser = (names, addressBlock) => {
  const resetParamElements = addressBlock.registers.map((register) => {
    return generateRegisterResetParams(register, names);
  });
  return {
    imports: ['chisel3._'],
    blocks: [endent`
      object ${names.userParamsObject.name} {
        def ${names.userParamsObject.deviceTreeName}: String = ${names.baseParamsObject.name}.${names.baseParamsObject.deviceTreeName}
        def ${names.userParamsObject.deviceTreeCompat}: Seq[String] = ${names.baseParamsObject.name}.${names.baseParamsObject.deviceTreeCompat}

        object ${names.userParamsObject.resetValues} {
          ${resetParamElements.join('\n\n')}
        }
      }
    `]
  };
};

const generateBlackBoxComponents = component => {
  const paramFields = [];

  // TODO only integer params supported
  paramSchema.reduce({
    leaf: (node, path) => {
      const name = path.join('_');
      const paramField = {
        name: name,
        type: 'Int'
      };
      if (node.default) {
        paramField['defaultValue'] = node.default;
      }
      paramFields.push(paramField);
    }
  })(component.pSchema);

  const blackboxParams = {
    className: `${component.name}Params`,
    fields: paramFields
  };

  return {
    imports: ['chisel3._'],
    blocks: [chisel.generateCaseClass(blackboxParams)],
    blackboxParams: blackboxParams
  };
};

const exportRegmap = comp => {
  const memoryMaps = comp.memoryMaps || [];
  const packageNameBase = `${comp.vendor}.${comp.library}.${comp.name}.regmap`;
  const blackboxParams = generateBlackBoxComponents(comp);
  blackboxParams.blackboxParams.packageName = packageNameBase;

  let baseResult = {};
  baseResult[`${comp.name}Params`] = endent`
    package ${packageNameBase}

    ${chisel.serializeImports(blackboxParams.imports)}

    ${blackboxParams.blocks.join('\n\n')}
  `;
  let userResult = {};
  memoryMaps.forEach((memoryMap) => {
    const addressBlocks = memoryMap.addressBlocks;

    let memoryMapUserResult = {};
    let memoryMapBaseResult = {};
    addressBlocks.forEach((addressBlock) => {
      const packageName = `${packageNameBase}.${memoryMap.name}.${addressBlock.name}`;
      const compatString = `"${comp.vendor},${comp.name}-${comp.version}"`;
      const deviceNameString = `"${comp.name}-${addressBlock.name}"`;
      const names = getAddressBlockNames(addressBlock);

      const base = generateBodyBase({
        names: names,
        addressBlock: addressBlock,
        addressUnitBits: memoryMap.addressUnitBits || 8,
        deviceNameString: deviceNameString,
        compatString: compatString,
        blackboxParams: blackboxParams.blackboxParams
      });
      const user = generateBodyUser(names, addressBlock);

      memoryMapBaseResult[`${addressBlock.name}-base`] = endent`
        // Generated Code
        // Please DO NOT EDIT
        package ${packageName}

        ${chisel.serializeImports(base.imports)}

        ${base.blocks.join('\n\n')}
      `;

      memoryMapUserResult[addressBlock.name] = endent`
        package ${packageName}

        ${chisel.serializeImports(user.imports)}

        ${user.blocks.join('\n\n')}
      `;
    });

    baseResult[memoryMap.name] = memoryMapBaseResult;
    userResult[memoryMap.name] = memoryMapUserResult;
  });

  return {
    base: baseResult,
    user: userResult
  };
};

const generateOMRegisterMap = (addressUnitBits, addressBlock) => {
  const registerMapElements = addressBlock.registers.map((register) => {
    const regFieldSeqElements = (register.fields || []).map(field => {
      const access = field.access || register.access || addressBlock.access;
      return `${field.bitOffset} -> ` + generateRegisterField(field, 'Bool()', access, field.resetValue);
    });

    const registerDesc = register.description ? `Some("${register.description}")` : 'None';

    if (addressUnitBits % 8 != 0) {
      throw new Error('addressUnitBits must be a multiple of 8.');
    }

    const addressOffset =
      addressUnitBits === 8 ? register.addressOffset : `${register.addressOffset} * ${addressUnitBits / 8}`;

    return endent`
      ${addressOffset} -> RegFieldGroup("${register.name}", ${registerDesc}, padFields(
        ${regFieldSeqElements}))`;
  });

  return endent`
    OMRegister.convert(
      ${registerMapElements.join(',\n')})`;
};

module.exports = {
  exportRegmap: exportRegmap,
  generateOMRegisterMap: generateOMRegisterMap,
  padFieldsMethod: padFieldsMethod
};
