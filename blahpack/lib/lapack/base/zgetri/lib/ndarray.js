

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Compute the inverse of a matrix using the LU factorization from zgetrf.
*
* @param {NonNegativeInteger} N - order of the matrix A
* @param {Complex128Array} A - input/output matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Int32Array} IPIV - pivot indices from zgetrf (0-based)
* @param {integer} strideIPIV - stride for IPIV
* @param {NonNegativeInteger} offsetIPIV - starting index for IPIV
* @param {Complex128Array} WORK - workspace array
* @param {integer} strideWORK - stride for WORK
* @param {NonNegativeInteger} offsetWORK - starting index for WORK
* @param {integer} lwork - length of the WORK array
* @returns {integer} status code (0 = success, k>0 if singular)
*/
function zgetri( N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV, WORK, strideWORK, offsetWORK, lwork ) { // eslint-disable-line max-len, max-params
	return base( N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV, WORK, strideWORK, offsetWORK, lwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zgetri;
