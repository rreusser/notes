
'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes an LU factorization of a general M-by-N complex matrix using partial pivoting with row interchanges (unblocked algorithm).
*
* @param {NonNegativeInteger} M - number of rows of matrix A
* @param {NonNegativeInteger} N - number of columns of matrix A
* @param {Complex128Array} A - input/output complex matrix (column-major)
* @param {integer} strideA1 - stride of the first dimension of `A` (complex elements)
* @param {integer} strideA2 - stride of the second dimension of `A` (complex elements)
* @param {NonNegativeInteger} offsetA - index offset for `A` (complex elements)
* @param {Int32Array} IPIV - pivot index output array, length min(M,N) (0-based)
* @param {integer} strideIPIV - stride for `IPIV`
* @param {NonNegativeInteger} offsetIPIV - index offset for `IPIV`
* @returns {integer} info - 0 if successful, k if U(k-1,k-1) is exactly zero (1-based singularity index)
*/
function zgetf2( M, N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV ) { // eslint-disable-line max-len, max-params
	return base( M, N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zgetf2;
