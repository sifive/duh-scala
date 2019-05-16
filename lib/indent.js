'use strict';

module.exports = (tab, sep) => {
  const prefix = ' '.repeat(tab);
  sep = (sep || '') + '\n';
  return str => {
    const lines = Array.isArray(str) ? str : [str];
    return lines
      .reduce((res, cur) => res.concat(cur.split('\n')), [])
      .map(line => prefix + line)
      .join(sep);
  };
};
