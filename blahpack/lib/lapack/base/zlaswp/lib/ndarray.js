'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Performs a series of row interchanges on a complex double-precision matrix.
*
* @param {PositiveInteger} N - number of columns in `A`
* @param {Complex128Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - index offset for `A` (in complex elements)
* @param {NonNegativeInteger} k1 - index of first row to interchange (0-based)
* @param {NonNegativeInteger} k2 - index of last row to interchange (0-based)
* @param {Int32Array} IPIV - vector of pivot indices (0-based)
* @param {integer} strideIPIV - `IPIV` stride length
* @param {NonNegativeInteger} offsetIPIV - index offset for `IPIV`
* @param {integer} incx - direction in which to apply pivots
* @returns {Complex128Array} permuted matrix `A`
*/
function zlaswp( N, A, strideA1, strideA2, offsetA, k1, k2, IPIV, strideIPIV, offsetIPIV, incx ) { // eslint-disable-line max-len, max-params
	return base( N, A, strideA1, strideA2, offsetA, k1, k2, IPIV, strideIPIV, offsetIPIV, incx );
}


// EXPORTS //

module.exports = zlaswp;
