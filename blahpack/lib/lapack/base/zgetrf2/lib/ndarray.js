'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes an LU factorization of a general complex M-by-N matrix using
* partial pivoting with row interchanges (recursive algorithm).
*
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Complex128Array} A - input/output complex matrix
* @param {integer} strideA1 - stride of the first dimension of A (complex elements)
* @param {integer} strideA2 - stride of the second dimension of A (complex elements)
* @param {NonNegativeInteger} offsetA - offset for A (complex elements)
* @param {Int32Array} IPIV - pivot index output array, length min(M,N) (0-based)
* @param {integer} strideIPIV - stride for IPIV
* @param {NonNegativeInteger} offsetIPIV - offset for IPIV
* @returns {integer} info - 0 if successful, k if U(k-1,k-1) is exactly zero (1-based)
*/
function zgetrf2( M, N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV ) { // eslint-disable-line max-len, max-params
	return base( M, N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV );
}


// EXPORTS //

module.exports = zgetrf2;
