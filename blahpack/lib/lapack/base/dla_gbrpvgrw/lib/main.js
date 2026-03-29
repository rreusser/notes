
'use strict';

// MODULES //

var dla_gbrpvgrw = require( './dla_gbrpvgrw.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

// NOTE: In stdlib, this uses setReadOnly from @stdlib/utils.

// For standalone development, we use Object.defineProperty directly.
Object.defineProperty( dla_gbrpvgrw, 'ndarray', {
	'value': ndarray,
	'enumerable': false,
	'writable': false,
	'configurable': false
});


// EXPORTS //

module.exports = dla_gbrpvgrw;
