/**
 * Solves a system of linear equations A_X = B or A^T_X = B with a general.
 * N-by-N matrix A using the LU factorization computed by dgetrf/dgetrf2.
 *
 * IPIV must contain 0-based pivot indices (as produced by dgetrf/dgetrf2).
 *
 *
 * @param {string} trans - `'no-transpose'` or `'transpose'`
 * @param {NonNegativeInteger} N - order of matrix A
 * @param {NonNegativeInteger} nrhs - number of right-hand side columns
 * @param {Float64Array} A - LU-factored N-by-N matrix (from dgetrf)
 * @param {integer} strideA1 - stride of the first dimension of A
 * @param {integer} strideA2 - stride of the second dimension of A
 * @param {NonNegativeInteger} offsetA - index offset for A
 * @param {Int32Array} IPIV - pivot indices from dgetrf (0-based)
 * @param {integer} strideIPIV - stride for IPIV
 * @param {NonNegativeInteger} offsetIPIV - index offset for IPIV
 * @param {Float64Array} B - right-hand side matrix, overwritten with solution
 * @param {integer} strideB1 - stride of the first dimension of B
 * @param {integer} strideB2 - stride of the second dimension of B
 * @param {NonNegativeInteger} offsetB - index offset for B
 * @throws {TypeError} First argument must be a valid transpose operation
 * @returns {integer} info - 0 if successful
 */

/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var isMatrixTranspose = require( '@stdlib/blas/base/assert/is-transpose-operation' );
var format = require( '@stdlib/string/format' );
var isTransposeOperation = require( '@stdlib/blas/base/assert/is-transpose-operation' );
var base = require( './base.js' );


// MAIN //

/**
* Solves a system of linear equations using LU factorization from DGETRF.
*
* @param {string} trans - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {integer} nrhs - number of right-hand sides
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Int32Array} IPIV - pivot indices
* @param {integer} strideIPIV - stride length for `IPIV`
* @param {NonNegativeInteger} offsetIPIV - starting index for `IPIV`
* @param {Float64Array} B - output matrix
* @param {integer} strideB1 - stride of the first dimension of `B`
* @param {integer} strideB2 - stride of the second dimension of `B`
* @param {NonNegativeInteger} offsetB - starting index for `B`
* @throws {TypeError} first argument must be a valid transpose operation
* @throws {RangeError} second argument must be a nonnegative integer
* @returns {integer} status code (0 = success)
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
*
* var A = new Float64Array( [ 2.0, 0.5, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0 ] );
* var IPIV = new Int32Array( [ 0, 1, 2 ] );
* var B = new Float64Array( [ 1.0, 1.0, 1.0 ] );
*
* var info = dgetrs( 'no-transpose', 3, 1, A, 1, 3, 0, IPIV, 1, 0, B, 1, 3, 0 );
* // info => 0
*/
function dgetrs( trans, N, nrhs, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV, B, strideB1, strideB2, offsetB ) {
	if ( !isMatrixTranspose( trans ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid transpose operation. Value: `%s`.', trans ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( N === 0 || nrhs === 0 ) {
		return 0;
	}
	return base( trans, N, nrhs, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV, B, strideB1, strideB2, offsetB );
}


// EXPORTS //

module.exports = dgetrs;
