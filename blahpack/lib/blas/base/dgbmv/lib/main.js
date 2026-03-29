
'use strict';

// MODULES //

var dgbmv = require( './dgbmv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

// NOTE: In stdlib, this uses setReadOnly from @stdlib/utils.

// For standalone development, we use Object.defineProperty directly.
Object.defineProperty( dgbmv, 'ndarray', {
	'value': ndarray,
	'enumerable': false,
	'writable': false,
	'configurable': false
});


// EXPORTS //

module.exports = dgbmv;
