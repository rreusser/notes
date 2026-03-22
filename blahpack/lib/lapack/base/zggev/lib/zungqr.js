'use strict';

// MODULES //

var zlarfb = require( '../../../../lapack/base/zlarfb/lib/base.js' );
var zlarft = require( '../../../../lapack/base/zlarft/lib/base.js' );
var zung2r = require( './zung2r.js' );

// VARIABLES //

var NB = 32;

// MAIN //

/**
* Generate an M-by-N complex matrix Q with orthonormal columns,
* which is defined as the first N columns of a product of K elementary
* reflectors of order M: Q = H(1)*H(2)*...*H(k).
*
* Blocked algorithm (ZUNGQR).
*
* Complex elements are stored as interleaved real/imaginary pairs.
* Matrix strides are in doubles.
*
* @private
* @param {NonNegativeInteger} M - rows
* @param {NonNegativeInteger} N - columns
* @param {NonNegativeInteger} K - number of reflectors
* @param {Float64Array} A - input/output matrix (interleaved complex)
* @param {integer} strideA1 - first dim stride of A (doubles)
* @param {integer} strideA2 - second dim stride of A (doubles)
* @param {NonNegativeInteger} offsetA - starting index for A
* @param {Float64Array} TAU - scalar factors (interleaved complex)
* @param {integer} strideTAU - stride for TAU (complex elements)
* @param {NonNegativeInteger} offsetTAU - starting index for TAU
* @param {Float64Array} WORK - workspace (interleaved complex)
* @param {integer} lwork - workspace size in complex elements
* @returns {integer} 0 on success
*/
function zungqr( M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, lwork ) { // eslint-disable-line max-len, max-params
	var ldwork;
	var idx;
	var sa1;
	var sa2;
	var oA;
	var nb;
	var kk;
	var ki;
	var ib;
	var i;
	var j;
	var l;

	if ( N <= 0 ) {
		return 0;
	}

	sa1 = strideA1;
	sa2 = strideA2;
	oA = offsetA;

	nb = NB;
	if ( nb > K ) {
		nb = K;
	}

	// Use unblocked code for small problems
	if ( nb < 2 || nb >= K ) {
		return zung2r( M, N, K, A, sa1, sa2, oA, TAU, strideTAU, offsetTAU, WORK );
	}

	// Use blocked code
	ki = Math.floor( ( K - 1 ) / nb ) * nb;
	kk = Math.min( K, ki + nb );

	// Set A(1:kk, kk+1:N) = 0
	for ( j = kk; j < N; j++ ) {
		for ( i = 0; i < kk; i++ ) {
			idx = oA + i * sa1 + j * sa2;
			A[ idx ] = 0.0;
			A[ idx + 1 ] = 0.0;
		}
	}

	// Apply unblocked code to trailing part
	if ( kk < N ) {
		zung2r( M - kk, N - kk, K - kk,
			A, sa1, sa2, oA + kk * sa1 + kk * sa2,
			TAU, strideTAU, offsetTAU + kk * strideTAU * 2,
			WORK );
	}

	ldwork = N;

	// Apply blocked code
	for ( i = ki; i >= 0; i -= nb ) {
		ib = Math.min( nb, K - i );

		if ( i + ib < N ) {
			// Form the triangular factor T
			// zlarft expects complex-element strides
			zlarft(
				'Forward', 'Columnwise', M - i, ib,
				A, sa1 / 2, sa2 / 2, oA + i * sa1 + i * sa2,
				TAU, strideTAU, offsetTAU + i * strideTAU * 2,
				WORK, 1, ib, ib * 2 * ldwork // T stored after work section
			);

			// Apply H to A(i:M, i+ib:N) from the left
			zlarfb(
				'Left', 'No transpose', 'Forward', 'Columnwise',
				M - i, N - i - ib, ib,
				A, sa1 / 2, sa2 / 2, oA + i * sa1 + i * sa2,
				WORK, 1, ib, ib * 2 * ldwork,
				A, sa1 / 2, sa2 / 2, oA + i * sa1 + ( i + ib ) * sa2,
				WORK, 1, ldwork, 0
			);
		}

		// Apply unblocked code to current block
		zung2r( M - i, ib, ib,
			A, sa1, sa2, oA + i * sa1 + i * sa2,
			TAU, strideTAU, offsetTAU + i * strideTAU * 2,
			WORK );

		// Set A(1:i, i:i+ib) = 0
		for ( j = i; j < i + ib; j++ ) {
			for ( l = 0; l < i; l++ ) {
				idx = oA + l * sa1 + j * sa2;
				A[ idx ] = 0.0;
				A[ idx + 1 ] = 0.0;
			}
		}
	}

	return 0;
}


// EXPORTS //

module.exports = zungqr;
