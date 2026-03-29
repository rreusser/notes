
'use strict';

// MODULES //

var dpbcon = require( './dpbcon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

// NOTE: In stdlib, this uses setReadOnly from @stdlib/utils.

// For standalone development, we use Object.defineProperty directly.
Object.defineProperty( dpbcon, 'ndarray', {
	'value': ndarray,
	'enumerable': false,
	'writable': false,
	'configurable': false
});


// EXPORTS //

module.exports = dpbcon;
