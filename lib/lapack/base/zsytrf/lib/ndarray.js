

/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
 * Compute the factorization of a complex symmetric matrix using Bunch-Kaufman diagonal pivoting.
 *
 *
 * @param {string} uplo - `'upper'` or `'lower'` indicating upper or lower triangular storage
 * @param {integer} N - order of the matrix
 * @param {Complex128Array} A - input/output matrix
 * @param {integer} strideA1 - first stride of A
 * @param {integer} strideA2 - second stride of A
 * @param {integer} offsetA - offset into A
 * @param {Int32Array} IPIV - output pivot indices
 * @param {integer} strideIPIV - stride of IPIV
 * @param {integer} offsetIPIV - offset into IPIV
 * @throws {TypeError} First argument must be a valid matrix triangle
 * @returns {integer} info value
 */
function zsytrf( uplo, N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV ) {
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( N === 0 ) {
		return;
	}
	return base( uplo, N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV );
}


// EXPORTS //

module.exports = zsytrf;
