'use strict';

const fileWriter = require('./file-writer.js');
const exportScalaBase = require('./export-scala-base.js');
const exportScalaUser = require('./export-scala-user.js');
const exportScalaRegMap = require('./export-scala-regmap.js');
const exportScalaMonitor = require('./export-scala-monitor.js');

exports.fileWriter = fileWriter;
exports.exportScalaBase = exportScalaBase;
exports.exportScalaUser = exportScalaUser;
exports.exportScalaRegMap = exportScalaRegMap.exportRegmap;
exports.exportScalaMonitor = exportScalaMonitor;
