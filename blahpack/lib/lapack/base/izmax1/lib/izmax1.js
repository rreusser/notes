
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {NonNegativeInteger} N - N
* @param {Complex128Array} ZX - ZX
* @param {integer} strideZX - strideZX
* @returns {*} result
*/
function izmax1( N, ZX, strideZX ) {
	var ozx;

	ozx = stride2offset( N, strideZX );
	return base( N, ZX, strideZX, ozx );
}


// EXPORTS //

module.exports = izmax1;
