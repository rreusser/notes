/**
 * Computes the minimum norm solution to a real linear least squares problem:.
 *
 * minimize 2-norm(|| b - A*x ||)
 *
 * using the singular value decomposition (SVD) of A. A is an M-by-N matrix
 * which may be rank-deficient.
 *
 * Several right hand side vectors b and solution vectors x can be handled
 * in a single call; they are stored as the columns of the M-by-NRHS right
 * hand side matrix B and the N-by-NRHS solution matrix X.
 *
 * The effective rank of A is determined by treating as zero those singular
 * values which are less than RCOND times the largest singular value.
 *
 *
 * @param {NonNegativeInteger} M - number of rows of A
 * @param {NonNegativeInteger} N - number of columns of A
 * @param {NonNegativeInteger} nrhs - number of right hand sides (columns of B)
 * @param {Float64Array} A - M-by-N matrix, overwritten on exit
 * @param {integer} strideA1 - stride of the first dimension of A
 * @param {integer} strideA2 - stride of the second dimension of A
 * @param {NonNegativeInteger} offsetA - starting index for A
 * @param {Float64Array} B - on entry, M-by-NRHS (or max(M,N)-by-NRHS) RHS matrix;
 * @param {integer} strideB1 - stride of the first dimension of B
 * @param {integer} strideB2 - stride of the second dimension of B
 * @param {NonNegativeInteger} offsetB - starting index for B
 * @param {Float64Array} S - output array of singular values in decreasing order (length min(M,N))
 * @param {integer} strideS - stride length for S
 * @param {NonNegativeInteger} offsetS - starting index for S
 * @param {number} rcond - used to determine the effective rank of A.
 * @param {Array} rank - output array; rank[0] set to the effective rank of A
 * @param {Float64Array} WORK - workspace array (if null, allocated internally)
 * @param {integer} strideWORK - stride length for WORK
 * @param {NonNegativeInteger} offsetWORK - starting index for WORK
 * @param {integer} lwork - length of WORK array (if 0 or WORK is null, auto-sized)
 * @returns {integer} info - 0 if successful, >0 if DBDSQR did not converge
 */

/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Computes the minimum norm solution to a real linear least squares problem:.
*
* @param {NonNegativeInteger} M - number of rows of A
* @param {NonNegativeInteger} N - number of columns of A
* @param {NonNegativeInteger} nrhs - number of right hand sides (columns of B)
* @param {Float64Array} A - M-by-N matrix, overwritten on exit
* @param {integer} strideA1 - stride of the first dimension of A
* @param {integer} strideA2 - stride of the second dimension of A
* @param {NonNegativeInteger} offsetA - starting index for A
* @param {Float64Array} B - on entry, M-by-NRHS (or max(M,N)-by-NRHS) RHS matrix;
* @param {integer} strideB1 - stride of the first dimension of B
* @param {integer} strideB2 - stride of the second dimension of B
* @param {NonNegativeInteger} offsetB - starting index for B
* @param {Float64Array} S - output array of singular values in decreasing order (length min(M,N))
* @param {integer} strideS - stride length for S
* @param {NonNegativeInteger} offsetS - starting index for S
* @param {number} rcond - used to determine the effective rank of A.
* @param {Array} rank - output array; rank[0] set to the effective rank of A
* @param {Float64Array} WORK - workspace array (if null, allocated internally)
* @param {integer} strideWORK - stride length for WORK
* @param {NonNegativeInteger} offsetWORK - starting index for WORK
* @param {integer} lwork - length of WORK array (if 0 or WORK is null, auto-sized)
* @throws {RangeError} first argument must be a nonnegative integer
* @throws {RangeError} second argument must be a nonnegative integer
* @throws {RangeError} third argument must be a nonnegative integer
* @returns {integer} info - 0 if successful, >0 if DBDSQR did not converge
*/
function dgelss( M, N, nrhs, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, S, strideS, offsetS, rcond, rank, WORK, strideWORK, offsetWORK, lwork ) {
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( nrhs < 0 ) {
		throw new RangeError( format( 'invalid argument. Third argument must be a nonnegative integer. Value: `%d`.', nrhs ) );
	}
	if ( M === 0 || N === 0 ) {
		return 0;
	}
	return base( M, N, nrhs, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, S, strideS, offsetS, rcond, rank, WORK, strideWORK, offsetWORK, lwork );
}


// EXPORTS //

module.exports = dgelss;
