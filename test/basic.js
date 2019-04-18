'use strict';

const chai = require('chai');
const lib = require('../lib/index.js');

const expect = chai.expect;

const comp = {
  component: {
    model: {
      ports: []
    },
    busInterfaces: []
  }
};

describe('basic', () => {
  it('empty-base', done => {
    expect(lib.exportScalaBase(comp)).to.be.a('string');
    done();
  });
  it('empty-user', done => {
    expect(lib.exportScalaUser(comp)).to.be.a('string');
    done();
  });
});

/* eslint-env mocha */
