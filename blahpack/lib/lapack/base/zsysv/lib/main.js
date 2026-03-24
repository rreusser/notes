

'use strict';

// MODULES //

var zsysv = require( './zsysv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

// NOTE: In stdlib, this uses setReadOnly from @stdlib/utils.
// For standalone development, we use Object.defineProperty directly.
Object.defineProperty( zsysv, 'ndarray', {
	value: ndarray,
	enumerable: false,
	writable: false,
	configurable: false
});


// EXPORTS //

module.exports = zsysv;
