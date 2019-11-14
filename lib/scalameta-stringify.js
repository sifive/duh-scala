'use strict';

const reduct = vals => vals.map(val => rec(val)).join(', ');

const rec = $ => {
  switch($.type) {
  case 'Pat.Wildcard':
    return '_';
  case 'Pat.Var':
    return rec($.name);
  case 'Defn.Val':
    return 'val ' + reduct($.pats) + ' = '+ rec($.rhs);
  case 'Term.Apply':
    return rec($.fun) + '(' + reduct($.args) + ')';
  case 'Term.ApplyInfix':
    return '(' + rec($.lhs) + ' ' + rec($.op) + ' ' + reduct($.args) + ')';
  case 'Lit.Int':
    return $.value;
  case 'Term.Name':
    return $.value;
  default:
    return $.type;
  }
};

module.exports = rec;
