'use strict';

// tools to deal with DUH parameter schema

const traverse = cb => {
  const enter = cb.enter || (() => {});
  const leave = cb.leave || (() => {});
  const leaf = cb.leaf || (() => {});

  const rec = path => node => {
    enter(node, path);
    if (node.type === 'object') {
      Object.keys(node.properties).map(key => {
        rec(path.concat(key))(node.properties[key]);
      });
    } else
    if (node.type === 'integer') {
      leaf(node, path);
    } else {
      leaf(node, path);
    }
    leave(node, path);
  };
  return rec([]);
};

const reduce = cb => {
  const enter = cb.enter || (() => {});
  const leave = cb.leave || (() => {});
  const leaf = cb.leaf || (() => {});

  let res = [];
  const rec = path => node => {
    if (typeof node !== 'object') {
      return res;
    }
    const eres = enter(node, path);
    res = res.concat((eres === undefined) ? [] : eres);
    if (node.type === 'object') {
      Object.keys(node.properties).map(key => {
        rec(path.concat(key))(node.properties[key]);
      });
    } else {
      const fres = leaf(node, path);
      res = res.concat((fres === undefined) ? [] : fres);
    }
    const lres = leave(node, path);
    res = res.concat((lres === undefined) ? [] : lres);
    return res;
  };
  return rec([]);
};

exports.traverse = traverse;
exports.reduce = reduce;
