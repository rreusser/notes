'use strict';

// MODULES //

var ztrsm = require( './ztrsm.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

Object.defineProperty( ztrsm, 'ndarray', {
	value: ndarray,
	enumerable: false,
	writable: false,
	configurable: false
});


// EXPORTS //

module.exports = ztrsm;
