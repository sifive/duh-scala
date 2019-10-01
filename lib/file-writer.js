'use strict';

const fs = require('fs-extra');
const scalametaParsers = require('scalameta-parsers');

const duhCore = require('duh-core');

const traverseSpec = require('../lib/traverse-spec');
const parseSource = scalametaParsers.parseSource;

module.exports = (emitter, argv) => new Promise (resolve => {
  const outputDir = argv.output;
  duhCore.readDuh(argv)
    .then(duhCore.expandAll)
    .then(duh => {
      const emittedRegMappers = emitter(duh.component);
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
