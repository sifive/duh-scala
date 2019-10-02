'use strict';

const paramSchema = require('./param-schema.js');
const traverseSpec = require('../lib/traverse-spec');
const flipString = require('../lib/flip-string');

const indent = require('./indent');
const get = require('lodash.get');
const axi4 = require('./axi4-tl');
const apb = require('./apb-scala');
const chisel = require('./chisel-utils');


const generators = {
  'amba.com': {
    AMBA4: {
      AXI4: axi4,
      AXI4Lite: axi4,
      APB4: apb
    }
  }
};

const generateBlackBoxComponents = (component, names) => {
  const paramsMap = {};
  const blackboxParamsMap = {};
  const blackboxIOParamsMap = {};
  const paramFields = [];

  // TODO only integer params supported
  paramSchema.reduce({
    leaf: (node, path) => {
      const name = path.join('_');
      paramsMap[name] = 'Int';
      blackboxParamsMap[name] = `core.IntParam(${name})`;
      blackboxIOParamsMap[name] = name;
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

  const blackboxParams = chisel.generateCaseClass({
    className: `${component.name}Params`,
    fields: paramFields
  });

  const blackboxIO = chisel.generateBundle({
    ports: component.model.ports,
    className: names.blackbox.io.name,
    paramsMap: paramsMap
  });

  const blackboxModule = chisel.generateBlackBox({
    className: names.blackbox.name,
    paramsMap: paramsMap,
    blackboxParamsMap: blackboxParamsMap,
    ioType: names.blackbox.io.name,
    ioParamsMap: blackboxIOParamsMap,
    desiredName: component.name
  });

  return {
    params: blackboxParams,
    bundle: blackboxIO,
    module: blackboxModule
  };
};

const generateMonitor = (component, names, generator) => {
  const bus = component.busInterfaces[0];
  const monitorNames = names.monitor;
  const generatorNames = generator.monitor.names;
  const paramsNames = monitorNames.paramsMethod;
  const connectNames = monitorNames.connectMethod;
  const legalizeNames = monitorNames.legalizeMethod;

  const blackboxArgs = {};
  paramSchema.reduce({
    leaf: (node, path) => {
      const name = path.join('_');
      blackboxArgs[name] = 'params.' + name;
    }
  })(component.pSchema);

  const portMap = ((bus.abstractionTypes || []).find(b => b.viewRef === 'RTLview') || {}).portMaps || {};

  const connects = [];
  traverseSpec({
    enter: (node, path) => {
      const duhName = flipString((path.length === 1) ? path[0] : (path[0] + path[path.length - 1]));
      const scalaName = path.join('.');

      if (portMap[duhName]) {
        connects.push(`blackbox.io.${portMap[duhName]} := ${connectNames.bundle}.${scalaName}`);
      }
    }
  })(generator.busDef);

  const blackboxArgsAssign = Object.entries(blackboxArgs).map(([key, value]) => {
    return `${key} = ${value}`;
  });

  const blackboxComponents = generateBlackBoxComponents(component, names);

  return {
    base: {
      imports: ['chisel3._', 'chisel3.core.Reset'].concat(generator.monitor.imports),
      body: `${blackboxComponents.params}

${blackboxComponents.bundle}

${blackboxComponents.module}

abstract class ${monitorNames.baseName}(${monitorNames.args}: ${generatorNames.args}) extends ${generatorNames.baseMonitor}(${monitorNames.args}) {
  def ${paramsNames.name}(${paramsNames.edgeParams}: ${generatorNames.edgeParams}): ${component.name}Params

  def ${connectNames.name}(
    ${connectNames.blackbox}: ${names.blackbox.name},
    ${connectNames.bundle}: ${generatorNames.bundle},
    ${connectNames.edgeParams}: ${generatorNames.edgeParams},
    ${connectNames.reset}: Reset): Unit = {
${indent(4)(connects)}
  }

  def ${legalizeNames.name}(
    ${legalizeNames.bundle}: ${generatorNames.bundle},
    ${legalizeNames.edgeParams}: ${generatorNames.edgeParams},
    ${legalizeNames.reset}: Reset): Unit = {
    val params = ${paramsNames.name}(${legalizeNames.edgeParams})
    val blackbox = Module(new ${names.blackbox.name}(
${indent(6, ',')(blackboxArgsAssign)}
    ))
    ${names.monitor.connectMethod.name}(blackbox, ${legalizeNames.bundle}, ${legalizeNames.edgeParams}, ${legalizeNames.reset})
  }
}`
    },
    user: {
      imports: ['chisel3._', 'chisel3.core.Reset'].concat(generator.monitor.imports),
      body: `class ${monitorNames.userName}(${names.monitor.args}: ${generatorNames.args}) extends ${names.monitor.baseName}(${names.monitor.args}) {
  // IMPLEMENT THIS METHOD: convert the input parameters to blackbox parameters
  def ${paramsNames.name}(${paramsNames.edgeParams}: ${generatorNames.edgeParams}): ${component.name}Params = {
  }

  // if you need to override the default connnection method: uncomment and implement this method
  /*
  override def ${connectNames.name}(
    ${connectNames.blackbox}: ${names.blackbox.name},
    ${connectNames.bundle}: ${generatorNames.bundle},
    ${connectNames.edgeParams}: ${generatorNames.edgeParams},
    ${connectNames.reset}: Reset): Unit = {
  }
  */
}`
    }
  };
};

const getNames = comp => {
  return {
    blackbox: {
      name: `${comp.name}BlackBox`,
      params: {
        name: `${comp.name}Params`
      },
      io: {
        name: `${comp.name}BlackBoxIO`
      }
    },
    monitor: {
      baseName: `${comp.name}MonitorBase`,
      userName: `${comp.name}Monitor`,
      args: 'params',
      paramsMethod: {
        name: 'getBlackBoxParams',
        edgeParams: 'edgeParams'
      },
      connectMethod: {
        name: 'connectBlackBoxPorts',
        edgeParams: 'edgeParams',
        bundle: 'bundle',
        blackbox: 'blackbox',
        reset: 'reset'
      },
      legalizeMethod: {
        name: 'legalize', // official rocket-chip name, do not change
        edgeParams: 'edgeParams',
        bundle: 'bundle',
        reset: 'reset'
      }
    }
  };
};

const getGenerator = component => {
  if (component.busInterfaces.length !== 1 || component.busInterfaces[0].interfaceMode !== 'monitor') {
    throw Error('duh component must have exactly one bus interface in monitor mode to generate a scala monitor');
  }

  const bus = component.busInterfaces[0];
  const busPath = [bus.busType.vendor, bus.busType.library, bus.busType.name];
  const generator = get(generators, busPath);

  if (!(generator && generator.monitor)) {
    throw new Error(`not a supported bus interface: ${busPath.join('.')}`);
  }

  return generator;
};

const exportMonitor = comp => {
  const names = getNames(comp);
  const generator = getGenerator(comp);
  const monitor = generateMonitor(comp, names, generator);

  const extraBaseImports = monitor.base.imports.map(i => {
    return `import ${i}`;
  });
  const extraUserImports = monitor.user.imports.map(i => {
    return `import ${i}`;
  });

  const base = `// Generated Code
// Please DO NOT EDIT
package ${comp.vendor}.${comp.library}.${comp.name}

${extraBaseImports.join('\n')}

${monitor.base.body}
`;

  const user = `package ${comp.vendor}.${comp.library}.${comp.name}

${extraUserImports.join('\n')}

${monitor.user.body}
`;
  const result = {
    base: {},
    user: {}
  };

  result.base[`${comp.name}-base`] = base;
  result.user[comp.name] = user;
  return result;
};

module.exports = exportMonitor;
