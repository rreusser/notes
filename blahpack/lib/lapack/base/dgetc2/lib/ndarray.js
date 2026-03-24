

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* LU factorization with complete pivoting of a general NxN matrix
*
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Int32Array} IPIV - input array
* @param {integer} strideIPIV - stride length for `IPIV`
* @param {NonNegativeInteger} offsetIPIV - starting index for `IPIV`
* @param {Int32Array} JPIV - output array
* @param {integer} strideJPIV - stride length for `JPIV`
* @param {NonNegativeInteger} offsetJPIV - starting index for `JPIV`
* @returns {integer} status code (0 = success)
*/
function dgetc2( N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV, JPIV, strideJPIV, offsetJPIV ) { // eslint-disable-line max-len, max-params
	return base( N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV, JPIV, strideJPIV, offsetJPIV ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dgetc2;
