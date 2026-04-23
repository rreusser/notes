/**
 * Generates one of the real orthogonal matrices Q or P^T determined by DGEBRD.
 * when reducing a real matrix A to bidiagonal form: `A = Q*B*P^T`.
 *
 * Q and P^T are defined as products of elementary reflectors H(i) or G(i)
 * respectively.
 *
 * If VECT = 'Q', A is assumed to have been an M-by-K matrix, and Q
 * is of order M:
 * if M >= K, Q = H(1) H(2) ... H(K) and dorgbr returns the first N
 * columns of Q, where M >= N >= K;
 * if M < K, Q = H(1) H(2) ... H(M-1) and dorgbr returns Q as an
 * M-by-M orthogonal matrix.
 *
 * If VECT = 'P', A is assumed to have been a K-by-N matrix, and P^T
 * is of order N:
 * if K < N, P^T = G(1) G(2) ... G(K) and dorgbr returns the first M
 * rows of P^T, where N >= M >= K;
 * if K >= N, P^T = G(1) G(2) ... G(N-1) and dorgbr returns P^T as an
 * N-by-N orthogonal matrix.
 *
 *
 * @param {string} vect - 'q' to generate Q, 'p' to generate P^T
 * @param {NonNegativeInteger} M - number of rows of the matrix Q or P^T
 * @param {NonNegativeInteger} N - number of columns of the matrix Q or P^T
 * @param {NonNegativeInteger} K - number of columns/rows in original matrix
 * @param {Float64Array} A - matrix containing reflectors from DGEBRD
 * @param {integer} strideA1 - stride of the first dimension of A
 * @param {integer} strideA2 - stride of the second dimension of A
 * @param {NonNegativeInteger} offsetA - starting index for A
 * @param {Float64Array} TAU - scalar factors of reflectors
 * @param {integer} strideTAU - stride for TAU
 * @param {NonNegativeInteger} offsetTAU - starting index for TAU
 * @param {Float64Array} WORK - workspace (ignored, allocated internally by dependencies)
 * @param {integer} strideWORK - stride for WORK (ignored)
 * @param {NonNegativeInteger} offsetWORK - starting index for WORK (ignored)
 * @returns {integer} info - 0 if successful
 */

/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Generates one of the real orthogonal matrices Q or P^T determined by DGEBRD.
*
* @param {string} vect - 'q' to generate Q, 'p' to generate P^T
* @param {NonNegativeInteger} M - number of rows of the matrix Q or P^T
* @param {NonNegativeInteger} N - number of columns of the matrix Q or P^T
* @param {NonNegativeInteger} K - number of columns/rows in original matrix
* @param {Float64Array} A - matrix containing reflectors from DGEBRD
* @param {integer} strideA1 - stride of the first dimension of A
* @param {integer} strideA2 - stride of the second dimension of A
* @param {NonNegativeInteger} offsetA - starting index for A
* @param {Float64Array} TAU - scalar factors of reflectors
* @param {integer} strideTAU - stride for TAU
* @param {NonNegativeInteger} offsetTAU - starting index for TAU
* @param {Float64Array} WORK - workspace (ignored, allocated internally by dependencies)
* @param {integer} strideWORK - stride for WORK (ignored)
* @param {NonNegativeInteger} offsetWORK - starting index for WORK (ignored)
* @throws {TypeError} first argument must be a valid vector type
* @throws {RangeError} second argument must be a nonnegative integer
* @throws {RangeError} third argument must be a nonnegative integer
* @throws {RangeError} fourth argument must be a nonnegative integer
* @returns {integer} info - 0 if successful
*/
function dorgbr( vect, M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK ) {
	if ( vect !== 'q' && vect !== 'p' ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid vector type. Value: `%s`.', vect ) );
	}
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Third argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( K < 0 ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be a nonnegative integer. Value: `%d`.', K ) );
	}
	if ( M === 0 || N === 0 ) {
		return 0;
	}
	return base(vect, M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK );
}


// EXPORTS //

module.exports = dorgbr;
