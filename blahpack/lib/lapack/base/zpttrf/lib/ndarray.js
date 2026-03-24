

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes the LDL^H factorization of a complex Hermitian positive definite tridiagonal matrix
*
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} d - input array
* @param {integer} strideD - stride length for `d`
* @param {NonNegativeInteger} offsetD - starting index for `d`
* @param {Float64Array} e - output array
* @param {integer} strideE - stride length for `e`
* @param {NonNegativeInteger} offsetE - starting index for `e`
* @returns {integer} status code (0 = success)
*/
function zpttrf( N, d, strideD, offsetD, e, strideE, offsetE ) { // eslint-disable-line max-len, max-params
	return base( N, d, strideD, offsetD, e, strideE, offsetE ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zpttrf;
