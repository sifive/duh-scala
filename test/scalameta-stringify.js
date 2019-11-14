'use strict';

const scalametaParsers = require('scalameta-parsers');
const stringify = require('../lib/scalameta-stringify.js');
const chai = require('chai');
const _ = require('../lib/scala-nodes.js');

// const $ = require('../lib/scala-nodes.js');

const expect = chai.expect;
const parseStat = scalametaParsers.parseStat;

describe('nodes', () => {
  [
    '(1 + 2)',
    '(1 + (200 * 31))',
    'Input(Bool())',
    'val foo, _ = Bool()'
  ].map(a => {
    it(a, done => {
      const b = parseStat.call({}, a);
      const c = stringify(b);
      console.log(JSON.stringify(b, null, 2));
      if (c !== a) {
        expect(c).to.eq(a);
      }
      done();
    });
  });
  it('nodes', done => {
    const b =
    _.Term.ApplyInfix(
      _.Lit.Int(5), '+', [_.Lit.Int(7)]
    );
    const c = stringify(b);
    console.log(c);
    done();
  })
});

/* eslint-env mocha */
