#!/usr/bin/env node
'use strict';

const path = require('path');
const fs = require('fs-extra');
const yargs = require('yargs');

const duhCore = require('duh-core');

const lib = require('../lib/index.js');

const argv = yargs
  .option('output', {
    alias: 'o',
    describe: 'output path for exported files',
    default: 'component/src'
  })
  .version()
  .help()
  .argv;

const flow = async argv => {
  if (argv.verbose) console.log('generate');
  const duh = await duhCore.readDuh(argv);
  const duh1 = await duhCore.expandAll(duh);
  // const moduleName = duh.component.name;
  const dir = argv.output;

  // generate .h file
  // const headerFile = `${dir}/${duh1.component.name}.h`;
  // await fs.outputFile(headerFile, genHeaderFile(duh1));

  // generate Scala wrapper
  const baseFile = `${dir}/${duh1.component.name}-base.scala`;
  await fs.outputFile(baseFile, lib.exportScalaBase(duh1));

  const userFile = `${dir}/${duh1.component.name}.scala`;
  if (!(await fs.pathExists(userFile))) {
    await fs.outputFile(userFile, lib.exportScalaUser(duh1));
  }
};

async function main(argv) {
  const cwd = process.cwd();
  const folderName = path.basename(cwd);
  const fileName = argv._[0] || folderName + '.json5';

  await flow({
    filename: fileName,
    output: argv.output
  });
}

main(argv);
