

'use strict';

// MODULES //

var zgetri = require( './zgetri.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

Object.defineProperty( zgetri, 'ndarray', {
	value: ndarray,
	enumerable: false,
	writable: false,
	configurable: false
});


// EXPORTS //

module.exports = zgetri;
