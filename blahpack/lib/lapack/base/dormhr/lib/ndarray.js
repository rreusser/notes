

'use strict';

// MODULES //

var isOperationSide = require( '@stdlib/blas/base/assert/is-operation-side' );
var isTransposeOperation = require( '@stdlib/blas/base/assert/is-transpose-operation' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
 * Overwrites the general real M-by-N matrix C with
 *
 *   SIDE = 'left'      SIDE = 'right'
 *   TRANS = 'no-transpose':    Q * C            C * Q
 *   TRANS = 'transpose':       Q^T * C          C * Q^T
 *
 * where Q is a real orthogonal matrix of order nq, with nq = M if
 * SIDE = 'left' and nq = N if SIDE = 'right'. Q is defined as the product of
 * IHI-ILO elementary reflectors, as returned by dgehrd:
 *
 *   Q = H(ilo) H(ilo+1) ... H(ihi-1)
 *
 *
 * @param {string} side - 'left' to apply Q from left, 'right' from right
 * @param {string} trans - 'no-transpose' for Q, 'transpose' for Q^T
 * @param {NonNegativeInteger} M - number of rows of C
 * @param {NonNegativeInteger} N - number of columns of C
 * @param {integer} ilo - 1-based lower index from dgehrd (1 <= ILO <= IHI)
 * @param {integer} ihi - 1-based upper index from dgehrd
 * @param {Float64Array} A - matrix containing reflectors from dgehrd
 * @param {integer} strideA1 - stride of the first dimension of A
 * @param {integer} strideA2 - stride of the second dimension of A
 * @param {NonNegativeInteger} offsetA - starting index for A
 * @param {Float64Array} TAU - scalar factors of the elementary reflectors
 * @param {integer} strideTAU - stride for TAU
 * @param {NonNegativeInteger} offsetTAU - starting index for TAU
 * @param {Float64Array} C - input/output M-by-N matrix
 * @param {integer} strideC1 - stride of the first dimension of C
 * @param {integer} strideC2 - stride of the second dimension of C
 * @param {NonNegativeInteger} offsetC - starting index for C
 * @param {Float64Array} WORK - workspace array
 * @param {integer} strideWORK - stride for WORK
 * @param {NonNegativeInteger} offsetWORK - starting index for WORK
 * @param {integer} lwork - dimension of WORK array
 * @throws {TypeError} First argument must be a valid operation side
 * @throws {TypeError} Second argument must be a valid transpose operation
 * @returns {integer} info - 0 if successful
 */
function dormhr( side, trans, M, N, ilo, ihi, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK, lwork ) { // eslint-disable-line max-len, max-params
	if ( !isOperationSide( side ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid operation side. Value: `%s`.', side ) );
	}
	if ( !isTransposeOperation( trans ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid transpose operation. Value: `%s`.', trans ) );
	}
	return base( side, trans, M, N, ilo, ihi, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK, lwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dormhr;
