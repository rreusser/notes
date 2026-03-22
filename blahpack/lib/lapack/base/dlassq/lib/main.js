

'use strict';

// MODULES //

var dlassq = require( './dlassq.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

// NOTE: In stdlib, this uses setReadOnly from @stdlib/utils.
// For standalone development, we use Object.defineProperty directly.
Object.defineProperty( dlassq, 'ndarray', {
	value: ndarray,
	enumerable: false,
	writable: false,
	configurable: false
});


// EXPORTS //

module.exports = dlassq;
