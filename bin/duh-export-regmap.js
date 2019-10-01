#!/usr/bin/env node
'use strict';

const path = require('path');
const yargs = require('yargs');

const lib = require('../lib/index.js');

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

const main = argv => {
  const cwd = process.cwd();
  const folderName = path.basename(cwd);
  const fileName = argv._[0] || folderName + '.json5';
  if (argv.verbose) console.log('regmap');
  lib.fileWriter(lib.exportScalaRegMap, Object.assign({filename: fileName}, argv));
};

main(argv);
/* eslint no-console: 0 */
