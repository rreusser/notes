/* eslint-disable camelcase */

'use strict';

// MODULES //

var zla_porpvgrw = require( './zla_porpvgrw.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

// NOTE: In stdlib, this uses setReadOnly from @stdlib/utils.
// For standalone development, we use Object.defineProperty directly.
Object.defineProperty( zla_porpvgrw, 'ndarray', { // eslint-disable-line no-restricted-syntax
	'value': ndarray,
	'enumerable': false,
	'writable': false,
	'configurable': false
});


// EXPORTS //

module.exports = zla_porpvgrw;
