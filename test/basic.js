'use strict';

const chai = require('chai');
const jsonRefs = require('json-refs');
const duhCore = require('duh-core');

const lib = require('../lib/index.js');

require('json5/lib/register');
const ark = require('@sifive/block-ark');

const expect = chai.expect;

const comp = {
  component: {
    model: {
      ports: []
    },
    busInterfaces: []
  }
};

describe('empty', () => {
  it('empty-base', done => {
    expect(lib.exportScalaBase(comp)).to.be.a('string');
    done();
  });
  it('empty-user', done => {
    expect(lib.exportScalaUser(comp)).to.be.a('string');
    done();
  });
});

describe('ark', () => {
  jsonRefs.resolveRefs(ark).then(res => {
    const duh1 = res.resolved;
    duhCore.expandAll(duh1).then(duh2 => {
      it('ark-base', done => {
        expect(lib.exportScalaBase(duh2)).to.be.a('string');
        done();
      });
      it('ark-user', done => {
        expect(lib.exportScalaUser(duh2)).to.be.a('string');
        done();
      });
    });
  });
});

/* eslint-env mocha */
