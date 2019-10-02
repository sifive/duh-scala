'use strict';

const fs = require('fs-extra');
const scalametaParsers = require('scalameta-parsers');

const duhCore = require('duh-core');

const traverseSpec = require('./traverse-spec');
const parseSource = scalametaParsers.parseSource;

/**
 * writes the output of a scala emitter to files
 *
 * Takes an emitter function that turns a duh component into scala source code.
 * The emitter should return an object with fields 'base' and 'user' that have
 * object values. The base and user objects source code trees where the leaves
 * are the string contnents of the file and the path to the leaves are the file
 * paths. The files specified by the base files alway be written, overriding
 * any existing files. The user files will only be written if those files do
 * not already exist. The argv.output specifies the root directory of the
 * emitted files. If argv.validate is set then the output scala code will be
 * checked for syntax errors.
 *
 * @param {Function} emitter emitter that emits scala code
 * @param {Object}   argv    object containing options
 */
module.exports = (emitter, argv) => new Promise (resolve => {
  const outputDir = argv.output;
  duhCore.readDuh(argv)
    .then(duhCore.expandAll)
    .then(duh => {
      const emittedContent = emitter(duh.component);
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
      writeFiles(true)(emittedContent.base);
      writeFiles(false)(emittedContent.user);
      resolve();
    });
});
/* eslint no-console: 0 */
