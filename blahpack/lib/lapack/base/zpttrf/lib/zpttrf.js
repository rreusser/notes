

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* Computes the L*D*L^H factorization of a complex Hermitian positive definite tridiagonal matrix.
*
* @param {NonNegativeInteger} N - N
* @param {Float64Array} d - d
* @param {integer} strideD - strideD
* @param {Complex128Array} e - e
* @param {integer} strideE - strideE
* @returns {*} result
*/
function zpttrf( N, d, strideD, e, strideE ) {
	var od;
	var oe;

	od = stride2offset( N, strideD );
	oe = stride2offset( N, strideE );
	return base( N, d, strideD, od, e, strideE, oe );
}


// EXPORTS //

module.exports = zpttrf;
