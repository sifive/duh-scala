#!/usr/bin/env node
'use strict';

const path = require('path');
const fs = require('fs-extra');
const yargs = require('yargs');
const scalametaParsers = require('scalameta-parsers');

const duhCore = require('duh-core');

const lib = require('../lib/index.js');
const traverseSpec = require('../lib/traverse-spec.js');

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

const flow = argv => new Promise (resolve => {
  const outputDir = argv.output;
  if (argv.verbose) console.log('regmap');
  duhCore.readDuh(argv)
    .then(duhCore.expandAll)
    .then(duh => {
      const emittedRegMappers = lib.exportScalaRegMap(duh.component);
      const writeFiles = (isBase) => traverseSpec({
        leaf: (node, path) => {
          const fileName = `${outputDir}/${path.join('/')}.scala`;
          fs.pathExists(fileName).then(exists => {
            if (!exists || isBase) {

              fs.outputFile(fileName, node);

              if (argv.validate) {
                const ast = parseSource.call({}, node);
                if (ast.error) {
                  console.error(ast);
                  throw new SyntaxError(ast.error);
                }
              }
            }
          });
        }
      });
      writeFiles(true)(emittedRegMappers.base);
      writeFiles(false)(emittedRegMappers.user);
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
