

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* Computes the L_D_L^T factorization of a real symmetric positive definite tridiagonal matrix A.
*
* @param {NonNegativeInteger} N - N
* @param {Float64Array} d - d
* @param {integer} strideD - strideD
* @param {Float64Array} e - e
* @param {integer} strideE - strideE
* @returns {*} result
*/
function dpttrf( N, d, strideD, e, strideE ) {
	var od;
	var oe;

	od = stride2offset( N, strideD );
	oe = stride2offset( N, strideE );
	return base( N, d, strideD, od, e, strideE, oe );
}


// EXPORTS //

module.exports = dpttrf;
