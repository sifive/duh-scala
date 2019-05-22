'use strict';

/*
    Change case for every character in the string
*/

module.exports = str =>
  str
    .toString().toUpperCase();
// .split('')
// .map(c => c['to' + (c === c.toUpperCase() ? 'Lower' : 'Upper') + 'Case']())
// .join('');
