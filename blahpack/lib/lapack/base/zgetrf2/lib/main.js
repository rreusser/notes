'use strict';

// MODULES //

var zgetrf2 = require( './zgetrf2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

Object.defineProperty( zgetrf2, 'ndarray', {
	value: ndarray,
	enumerable: false,
	writable: false,
	configurable: false
});


// EXPORTS //

module.exports = zgetrf2;
