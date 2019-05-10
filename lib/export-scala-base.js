'use strict';

const get = require('lodash.get');
const axi4Tl = require('./axi4-tl');
const apb = require('./apb-scala');
const ahblite = require('./ahblite-scala');
const intcTl = require('./intc-tl');
const exportScalaChannel = require('./export-scala-channel');
const signValue = require('./sign-value');
const traverseSpec = require('./traverse-spec');
const flipString = require('./flip-string');
const indent = require('./indent');
const parts = require('./export-scala-parts.js');
const bundler = require('./export-scala-bundle.js');
const type = require('./scala-type.js');
const paramSchema = require('./param-schema.js');

const generators = {
  AXI4: axi4Tl,
  AXI4Lite: axi4Tl,
  APB4: apb,
  AHBLite: ahblite,
  interrupts: intcTl,
  channel: exportScalaChannel
};

const buses = {
  'amba.com': {
    AMBA3: {
      AHBLite: ahblite.busDef
    },
    AMBA4: {
      AXI4: axi4Tl.busDef,
      AXI4Lite: axi4Tl.busDef,
      APB4: apb.busDef
    }
  },
  'sifive.com': {
    basic: {
      channel: exportScalaChannel.busDef
    }
  }
};

const getBusName = path => flipString(
  (path.length === 1) ? path[0] : (path[0] + path[path.length - 1]));

const perPort = e => {

  const dir = ({
    in: 'Input',
    out: 'Output'
  })[e.wire.direction];

  return `  val ${e.name} = ${dir}(${type(e.wire.width)})`;
};

const perBusInterface = comp => e => {
  const handle = get(
    generators,
    [e.busType.name, e.interfaceMode, 'adapter'],
    () => () => `BundleBridgeSource(() => new ${e.name}Bundle)`
    // () => () => '0 // busType: ' + e.busType.name + ', mode: ' + e.interfaceMode + '\n'
  );
  return `  val ${e.name}Node = ${handle(comp)(e)}`;
};

const perBusInterfaceAlias = () => e =>
  `val (${e.name}0, _) = ${e.name}Node.${
    e.interfaceMode == 'slave' ? 'in' : 'out'
  }(0)`;

const perBusInterfaceParams = comp => e => get(
  generators, [e.busType.name, e.interfaceMode, 'params'],
  () => e => `case class P${e.name}Params(burstBytes: Int) // name: ${e.busType.name}, mode: ${e.interfaceMode}`
)(comp)(e);

const blackWire = {
  out: (lhs, rhs) => lhs + ' := blackbox.io.' + rhs,
  in:  (lhs, rhs) => 'blackbox.io.' + rhs + ' := ' + lhs
};

const perBusInterfaceWiring = comp => {
  const portObj = comp.model.ports.reduce((prev, cur) => {
    prev[cur.name] = cur;
    return prev;
  }, {});

  return e => {
    let res = '';
    res += '// wiring for ' + e.name + ' of type ' + e.busType.name + '\n';
    const bus = get(
      buses,
      [e.busType.vendor, e.busType.library, e.busType.name]
    );
    if (bus === undefined) {
      return res;
    }

    const lPortMap = e.abstractionTypes[0].portMaps;

    traverseSpec({
      enter: (node, path) => {
        if (Array.isArray(node)) {
          res += Object
            .keys(lPortMap)
            .map((busName, i) => {
              const rhs = lPortMap[busName]; // .physicalPort.name;
              blackWire[portObj[rhs].wire.direction](
                e.name + '0(' + i + ')',
                rhs
              );
            })
            .join('\n');
          return;
        }

        if (typeof node === 'object') {
          res += path[0] ? '// ' + path[0] + '\n' : '';
          return;
        }

        const busName = getBusName(path);
        const tlName = path.join('.');

        if (lPortMap[busName]) {
          const rhs = lPortMap[busName]; // .physicalPort.name;
          res += blackWire[portObj[rhs].wire.direction](
            e.name + '0.' + tlName,
            rhs
          ) + '\n';
          return;
        }

        const sig = signValue(node);

        if (sig.sign ^ (e.interfaceMode == 'slave')) {
          res += `${e.name}0.${tlName} := ${
            sig.value === 1 ? 'true.B' : '0.U'
          }\n`;
          return;
        }
        res += '// ' + busName + '\n';
      }
    })(bus);
    return res;
  };
};

const perBundlePortWiring = () => e => {
  const h1 = 'ioBridgeSource.bundle.' + e.name;
  const h2 = 'blackbox.io.' + e.name;
  if (e.wire.direction === 'out') {
    return h1 + ' := ' + h2;
  }
  return h2 + ' := ' + h1;
};

const perBusInterfaceWiring2 = comp => e => get(
  generators,
  [e.busType.name, e.interfaceMode, 'wiring'],
  () => () => `// busType: ${e.busType.name}, mode: ${e.interfaceMode}`
)(comp)(e);

const perBusInterfaceBundle = comp => e => get(
  generators,
  [e.busType.name, e.interfaceMode, 'adapter']
)
  ? `// busType: ${e.busType.name}, mode: ${e.interfaceMode}`
  : `
class ${e.name}Bundle() extends Bundle {
${indent(2)(bundler(comp)(e))}
}`;

module.exports = p => {
  const comp = p.component;
  return `// Generated Code
// Please DO NOT EDIT

${parts.header(comp)}

class ${comp.name}BlackBoxIO(
${indent(2)(paramSchema.reduce({
    leaf: (node, path) => `val ${path.join('_')}: Int`
  })(comp.pSchema).join(',\n'))}
) extends Bundle {
${comp.model.ports.map(perPort).join('\n')}
}

class ${comp.name}(
${indent(2)(paramSchema.reduce({
    leaf: (node, path) => `val ${path.join('_')}: Int`
  })(comp.pSchema).join(',\n'))}
) extends BlackBox(Map(
${indent(2)(paramSchema.reduce({
    leaf: (node, path) => `"${path.join('_')}" -> core.IntParam(${path.join('_')})`
  })(comp.pSchema).join(',\n'))}
)) with HasBlackBoxResource {
  val io = IO(new ${comp.name}BlackBoxIO(
${indent(4)(paramSchema.reduce({
    leaf: (node, path) => path.join('_')
  })(comp.pSchema).join(',\n'))}
))
// setResource("top.v")
}

case class ${comp.name}Params(
${indent(2)(paramSchema.reduce({
    leaf: (node, path) => `${path.join('_')}: Int${node.default ? ' = ' + node.default : ''}`
  })(comp.pSchema).join(',\n'))}
)

${comp.busInterfaces.map(perBusInterfaceBundle(comp)).join('\n')}

class L${comp.name}Base(c: ${
  comp.name
}Params)(implicit p: Parameters) extends LazyModule {
  val device = new SimpleDevice("${comp.name}", Seq("sifive,${comp.name}-v0"))

${comp.busInterfaces.map(perBusInterface(comp)).join('\n')}

  val ioBridgeSource = BundleBridgeSource(() => new ${comp.name}BlackBoxIO(
${indent(4)(paramSchema.reduce({
    leaf: (node, path) => 'c.' + path.join('_')
  })(comp.pSchema).join(',\n'))}
  ))

  class L${comp.name}BaseImp extends LazyRawModuleImp(this) {
    val blackbox = Module(new ${comp.name}(
${indent(6)(paramSchema.reduce({
    leaf: (node, path) => 'c.' + path.join('_')
  })(comp.pSchema).join(',\n'))}
    ))
    // interface wiring 2
${indent(4)(comp.busInterfaces.map(perBusInterfaceWiring2(comp)).join('\n'))}
    // port wiring
${indent(4)(comp.model.ports.map(perBundlePortWiring(comp)).join('\n'))}
    // interface alias
${indent(4)(comp.busInterfaces.map(perBusInterfaceAlias(comp)).join('\n'))}
    // interface wiring
${indent(4)(comp.busInterfaces.map(perBusInterfaceWiring(comp)).join('\n'))}

  }

  lazy val module = new L${comp.name}BaseImp
}

${comp.busInterfaces.map(perBusInterfaceParams(comp)).join('\n')}

case class N${comp.name}TopParams(
    blackbox: ${comp.name}Params,
${indent(4)(
    comp.busInterfaces.map(e => `${e.name}Params: P${e.name}Params`).join(',\n')
  )}
) {
  def setBurstBytes(x: Int): N${comp.name}TopParams = this.copy(
${indent(4)(
    comp.busInterfaces
      .map(e => `${e.name}Params = ${e.name}Params.copy(burstBytes = x)`)
      .join(',\n')
  )}
  )
}

object N${comp.name}TopParams {
  def defaults(

  ) =
    N${comp.name}TopParams(
      blackbox = ${comp.name}Params(

      ),
${indent(6)(
    comp.busInterfaces
      .map(e => `${e.name}Params = P${e.name}Params(burstBytes = burstBytes)`)
      .join(',\n')
  )}
    )
}

class N${comp.name}TopBase(c: N${
  comp.name
}TopParams)(implicit p: Parameters) extends SimpleLazyModule
{
  val imp = LazyModule(new L${comp.name}(c.blackbox))
  ${get(generators, 'channel.common.node', () => '')(comp)}
${indent(2)(
    comp.busInterfaces
      .map(e => get(
        generators,
        [e.busType.name, e.interfaceMode, 'node'],
        () => () => `// busType: ${e.busType.name}, mode: ${e.interfaceMode}`
      )(comp)(e))
      .join('\n')
  )}
}

object N${comp.name}TopBase {
  def attach(c: N${comp.name}TopParams)(bap: BlockAttachParams): N${
  comp.name
}Top = {
    implicit val p: Parameters = bap.p
    val ${comp.name}_top = LazyModule(new N${comp.name}Top(c))
    ${get(generators, 'channel.common.attach', () => '')(comp)}
${indent(4)(
    comp.busInterfaces
      .map(e => get(
        generators,
        [e.busType.name, e.interfaceMode, 'attach'],
        () => () => `// busType: ${e.busType.name}, mode: ${e.interfaceMode}`
      )(comp)(e))
      .join('\n')
  )}
    ${comp.name}_top
  }
}

class With${comp.name}TopBase (
${indent(2)(
    comp
      .busInterfaces
      .filter(e => e.interfaceMode === 'slave')
      .filter(e => ({AXI4: 1, AXI4Lite: 1, APB4: 1, AHBLite: 1})[e.busType.name])
      .map(e => e.name + '_base: BigInt')
      .join(',\n'))}
) extends Config((site, here, up) => {
  case BlockDescriptorKey =>
    BlockDescriptor(
      name = "${comp.name}",
      place = N${comp.name}Top.attach(N${
  comp.name
}TopParams.defaults(burstBytes = site(CacheBlockBytes)))) +: up(BlockDescriptorKey, site)
})


`;
};
