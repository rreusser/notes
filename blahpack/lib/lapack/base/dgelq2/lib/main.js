

'use strict';

// MODULES //

var dgelq2 = require( './dgelq2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

// NOTE: In stdlib, this uses setReadOnly from @stdlib/utils.
// For standalone development, we use Object.defineProperty directly.
Object.defineProperty( dgelq2, 'ndarray', {
	value: ndarray,
	enumerable: false,
	writable: false,
	configurable: false
});


// EXPORTS //

module.exports = dgelq2;
