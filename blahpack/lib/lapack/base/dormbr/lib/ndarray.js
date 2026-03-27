/**
 * If VECT = 'Q', overwrite the matrix C with one of:.
 *
 * ```text
 * SIDE = 'L'     SIDE = 'R'
 * TRANS = 'N':  Q*C          C*Q
 * TRANS = 'T':  Q^T*C        C*Q^T
 * ```
 *
 * If VECT = 'P', overwrite the matrix C with one of:
 *
 * ```text
 * SIDE = 'L'     SIDE = 'R'
 * TRANS = 'N':  P*C          C*P
 * TRANS = 'T':  P^T*C        C*P^T
 * ```
 *
 * Here Q and P^T are the orthogonal matrices determined by DGEBRD when
 * reducing a real matrix A to bidiagonal form: `A = Q*B*P^T`.
 * Q is defined as a product of elementary reflectors H(i) = I - tauq(i)_v(i)_v(i)^T.
 * P is defined as a product of elementary reflectors G(i) = I - taup(i)_u(i)_u(i)^T.
 *
 *
 * @param {string} vect - `'apply-Q'` or `'apply-P'`
 * @param {string} side - `'left'` or `'right'`
 * @param {string} trans - `'no-transpose'` or `'transpose'`
 * @param {NonNegativeInteger} M - number of rows of C
 * @param {NonNegativeInteger} N - number of columns of C
 * @param {NonNegativeInteger} K - number of columns/rows in original matrix for DGEBRD
 * @param {Float64Array} A - matrix containing reflectors from DGEBRD
 * @param {integer} strideA1 - stride of the first dimension of A
 * @param {integer} strideA2 - stride of the second dimension of A
 * @param {NonNegativeInteger} offsetA - starting index for A
 * @param {Float64Array} TAU - scalar factors of reflectors (TAUQ or TAUP)
 * @param {integer} strideTAU - stride for TAU
 * @param {NonNegativeInteger} offsetTAU - starting index for TAU
 * @param {Float64Array} C - input/output matrix
 * @param {integer} strideC1 - stride of the first dimension of C
 * @param {integer} strideC2 - stride of the second dimension of C
 * @param {NonNegativeInteger} offsetC - starting index for C
 * @param {Float64Array} WORK - workspace
 * @param {integer} strideWORK - stride for WORK
 * @param {NonNegativeInteger} offsetWORK - starting index for WORK
 * @throws {TypeError} Second argument must be a valid operation side
 * @throws {TypeError} Third argument must be a valid transpose operation
 * @returns {integer} info - 0 if successful
 */

/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var isMatrixTranspose = require( '@stdlib/blas/base/assert/is-transpose-operation' );
var isOperationSide = require( '@stdlib/blas/base/assert/is-operation-side' );
var format = require( '@stdlib/string/format' );
var isTransposeOperation = require( '@stdlib/blas/base/assert/is-transpose-operation' );
var base = require( './base.js' );


// MAIN //

/**
* If VECT = 'Q', overwrite the matrix C with one of:.
*
* @param {string} vect - `'apply-Q'` or `'apply-P'`
* @param {string} side - `'left'` or `'right'`
* @param {string} trans - `'no-transpose'` or `'transpose'`
* @param {NonNegativeInteger} M - number of rows of C
* @param {NonNegativeInteger} N - number of columns of C
* @param {NonNegativeInteger} K - number of columns/rows in original matrix for DGEBRD
* @param {Float64Array} A - matrix containing reflectors from DGEBRD
* @param {integer} strideA1 - stride of the first dimension of A
* @param {integer} strideA2 - stride of the second dimension of A
* @param {NonNegativeInteger} offsetA - starting index for A
* @param {Float64Array} TAU - scalar factors of reflectors (TAUQ or TAUP)
* @param {integer} strideTAU - stride for TAU
* @param {NonNegativeInteger} offsetTAU - starting index for TAU
* @param {Float64Array} C - input/output matrix
* @param {integer} strideC1 - stride of the first dimension of C
* @param {integer} strideC2 - stride of the second dimension of C
* @param {NonNegativeInteger} offsetC - starting index for C
* @param {Float64Array} WORK - workspace
* @param {integer} strideWORK - stride for WORK
* @param {NonNegativeInteger} offsetWORK - starting index for WORK
* @throws {TypeError} first argument must be a valid vector type
* @throws {TypeError} second argument must be a valid operation side
* @throws {TypeError} third argument must be a valid transpose operation
* @throws {RangeError} fourth argument must be a nonnegative integer
* @throws {RangeError} fifth argument must be a nonnegative integer
* @throws {RangeError} sixth argument must be a nonnegative integer
* @returns {integer} info - 0 if successful
*/
function dormbr( vect, side, trans, M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK ) {
	if ( vect !== 'q' && vect !== 'p' ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid vector type. Value: `%s`.', vect ) );
	}
	if ( !isOperationSide( side ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid operation side. Value: `%s`.', side ) );
	}
	if ( !isMatrixTranspose( trans ) ) {
		throw new TypeError( format( 'invalid argument. Third argument must be a valid transpose operation. Value: `%s`.', trans ) );
	}
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Fifth argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( K < 0 ) {
		throw new RangeError( format( 'invalid argument. Sixth argument must be a nonnegative integer. Value: `%d`.', K ) );
	}
	if ( M === 0 || N === 0 ) {
		return 0;
	}
	return base(vect, side, trans, M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK );
}


// EXPORTS //

module.exports = dormbr;
