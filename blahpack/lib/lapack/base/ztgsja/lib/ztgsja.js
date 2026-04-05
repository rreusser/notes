
'use strict';

// MODULES //

var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Computes the generalized singular value decomposition of two complex upper triangular matrices.
*
* ## Notes
*
* -   This is a placeholder for a future BLAS/LAPACK-style API (order/layout, LDA). Use `ztgsja.ndarray` for the strided interface.
*
* @param {string} jobu - specifies the operation type
* @param {string} jobv - specifies the operation type
* @param {string} jobq - specifies the operation type
* @param {NonNegativeInteger} M - number of rows of A
* @param {integer} p - number of rows of B
* @param {NonNegativeInteger} N - number of columns of A and B
* @param {NonNegativeInteger} K - dimension of the first block
* @param {integer} l - dimension of the second block
* @param {Complex128Array} A - M-by-N matrix A (overwritten)
* @param {integer} LDA - leading dimension of A
* @param {Complex128Array} B - P-by-N matrix B (overwritten)
* @param {integer} LDB - leading dimension of B
* @param {number} tola - convergence tolerance for A
* @param {number} tolb - convergence tolerance for B
* @param {Float64Array} ALPHA - output array for generalized singular values (length N)
* @param {Float64Array} BETA - output array for generalized singular values (length N)
* @param {Complex128Array} U - M-by-M unitary matrix U
* @param {integer} LDU - leading dimension of U
* @param {Complex128Array} V - P-by-P unitary matrix V
* @param {integer} LDV - leading dimension of V
* @param {Complex128Array} Q - N-by-N unitary matrix Q
* @param {integer} LDQ - leading dimension of Q
* @param {Complex128Array} WORK - workspace array
* @param {Int32Array} ncycle - output: ncycle[0] receives the number of cycles
* @returns {integer} info - 0 for success, 1 if not converged
*/
function ztgsja( jobu, jobv, jobq, M, p, N, K, l, A, LDA, B, LDB, tola, tolb, ALPHA, BETA, U, LDU, V, LDV, Q, LDQ, WORK, ncycle ) { // eslint-disable-line max-len, max-params, no-unused-vars
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Sixth argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( K < 0 ) {
		throw new RangeError( format( 'invalid argument. Seventh argument must be a nonnegative integer. Value: `%d`.', K ) );
	}
	return base( jobu, jobv, jobq, M, p, N, K, l, A, 1, LDA, 0, B, 1, LDB, 0, tola, tolb, ALPHA, 1, 0, BETA, 1, 0, U, 1, LDU, 0, V, 1, LDV, 0, Q, 1, LDQ, 0, WORK, 1, 0, ncycle ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = ztgsja;
