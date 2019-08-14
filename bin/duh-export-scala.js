#!/usr/bin/env node
'use strict';

const path = require('path');
const fs = require('fs-extra');
const yargs = require('yargs');
const scalametaParsers = require('scalameta-parsers');

const duhCore = require('duh-core');

const lib = require('../lib/index.js');

const parseSource = scalametaParsers.parseSource;

const argv = yargs
  .option('output', {
    alias: 'o',
    describe: 'output path for exported files',
    default: 'component/src'
  })
  .option('validate', {
    alias: 'val',
    describe: 'validate output Scala',
    default: false,
    type: 'boolean'
  })
  .version()
  .help()
  .argv;

const fixupPortDirections = duh => new Promise((resolve) => {
  if (duh.component !== undefined) {
    const model = duh.component.model;
    model.ports = model.ports.map(port => {
      const wire = port.wire;
      wire.direction = wire.analog ? wire.analog : wire.direction;
      return port;
    });
  }

  resolve(duh);
});

const flow = argv => new Promise (resolve => {
  const dir = argv.output;
  if (argv.verbose) console.log('generate');
  duhCore.readDuh(argv)
    .then(duhCore.expandAll)
    .then(fixupPortDirections)
    .then(duh1 => {
      // const moduleName = duh.component.name;
      // generate .h file
      // const headerFile = `${dir}/${duh1.component.name}.h`;
      // await fs.outputFile(headerFile, genHeaderFile(duh1));

      {
        // generate Scala wrapper
        const name = `${dir}/${duh1.component.name}-base.scala`;
        const body = lib.exportScalaBase(duh1);
        fs.outputFile(name, body);
        if (argv.validate) {
          const ast = parseSource.call({}, body);
          if (ast.error) {
            console.error(ast);
            const e = new SyntaxError(ast.error);
            throw e;
          }
        }
      }
      {
        const name = `${dir}/${duh1.component.name}.scala`;
        fs.pathExists(name).then(exists => {
          if (!exists) {
            const body = lib.exportScalaUser(duh1);
            fs.outputFile(name, body);
            if (argv.validate) {
              const ast = parseSource.call({}, body);
              if (ast.error) {
                console.error(ast);
                const e = new SyntaxError(ast.error);
                throw e;
              }
            }
          }
        });
      }
      resolve();
    });
});

const main = argv => {
  const cwd = process.cwd();
  const folderName = path.basename(cwd);
  const fileName = argv._[0] || folderName + '.json5';
  flow(Object.assign({filename: fileName}, argv));
};

main(argv);
/* eslint no-console: 0 */
