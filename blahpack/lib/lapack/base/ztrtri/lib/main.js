

'use strict';

// MODULES //

var ztrtri = require( './ztrtri.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

Object.defineProperty( ztrtri, 'ndarray', {
	value: ndarray,
	enumerable: false,
	writable: false,
	configurable: false
});


// EXPORTS //

module.exports = ztrtri;
