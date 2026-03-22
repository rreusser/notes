'use strict';

// MODULES //

var zlarfb = require( '../../../../lapack/base/zlarfb/lib/base.js' );
var zlarft = require( '../../../../lapack/base/zlarft/lib/base.js' );
var zunm2r = require( './zunm2r.js' );

// VARIABLES //

var NB = 32; // Block size

// MAIN //

/**
* Apply a complex orthogonal matrix Q (from QR factorization) to a
* complex M-by-N matrix C: C := Q*C, Q^H*C, C*Q, or C*Q^H.
*
* Q = H(1)*H(2)*...*H(k) where H(i) are Householder reflectors stored
* in columns of A below the diagonal, with scalar factors in TAU.
*
* Complex elements are stored as interleaved real/imaginary pairs.
* Matrix strides are in doubles.
*
* @private
* @param {string} side - 'L' or 'R'
* @param {string} trans - 'N' or 'C'
* @param {NonNegativeInteger} M - rows of C
* @param {NonNegativeInteger} N - columns of C
* @param {NonNegativeInteger} K - number of reflectors
* @param {Float64Array} A - reflector matrix from ZGEQRF (interleaved complex)
* @param {integer} strideA1 - first dim stride of A (doubles)
* @param {integer} strideA2 - second dim stride of A (doubles)
* @param {NonNegativeInteger} offsetA - starting index for A
* @param {Float64Array} TAU - scalar factors (interleaved complex)
* @param {integer} strideTAU - stride for TAU (complex elements)
* @param {NonNegativeInteger} offsetTAU - starting index for TAU
* @param {Float64Array} C - matrix (interleaved complex, modified in-place)
* @param {integer} strideC1 - first dim stride of C (doubles)
* @param {integer} strideC2 - second dim stride of C (doubles)
* @param {NonNegativeInteger} offsetC - starting index for C
* @param {Float64Array} WORK - workspace (interleaved complex, length >= 2*NB*(NB+nw))
* @param {integer} lwork - workspace size in complex elements
* @returns {integer} 0 on success
*/
function zunmqr( side, trans, M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, C, strideC1, strideC2, offsetC, WORK, lwork ) { // eslint-disable-line max-len, max-params
	var notran;
	var ldwork;
	var left;
	var nw;
	var nb;
	var mi;
	var ni;
	var ic;
	var jc;
	var nq;
	var ib;
	var i1;
	var i2;
	var i3;
	var iwt;
	var ldt;
	var i;

	if ( M === 0 || N === 0 || K === 0 ) {
		return 0;
	}

	left = ( side === 'L' || side === 'l' );
	notran = ( trans === 'N' || trans === 'n' );

	if ( left ) {
		nq = M;
		nw = Math.max( 1, N );
	} else {
		nq = N;
		nw = Math.max( 1, M );
	}

	nb = NB;
	if ( nb > K ) {
		nb = K;
	}

	// If block size is too large or equals K, use unblocked algorithm
	if ( nb < 2 || nb >= K ) {
		return zunm2r( side, trans, M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, C, strideC1, strideC2, offsetC, WORK );
	}

	ldwork = nw;
	ldt = nb + 1;

	// T matrix goes at offset iwt in WORK (in complex elements, so multiply by 2 for doubles)
	iwt = nw * nb; // complex offset

	// Determine iteration direction
	if ( ( left && !notran ) || ( !left && notran ) ) {
		i1 = 0;
		i2 = K;
		i3 = nb;
	} else {
		i1 = Math.floor( ( K - 1 ) / nb ) * nb;
		i2 = -1;
		i3 = -nb;
	}

	if ( left ) {
		ni = N;
		jc = 0;
	} else {
		mi = M;
		ic = 0;
	}

	for ( i = i1; ( i3 > 0 ) ? ( i < i2 ) : ( i > i2 ); i += i3 ) {
		ib = Math.min( nb, K - i );

		// Form the triangular factor of the block reflector
		// H = H(i) H(i+1) ... H(i+ib-1)
		// zlarft expects complex-element strides
		zlarft(
			'Forward', 'Columnwise', nq - i, ib,
			A, strideA1 / 2, strideA2 / 2, offsetA + i * strideA1 + i * strideA2,
			TAU, strideTAU, offsetTAU + i * strideTAU * 2,
			WORK, 1, ldt, iwt * 2 // T stored at WORK[iwt*2] with stride1=1, stride2=ldt
		);

		if ( left ) {
			mi = M - i;
			ic = i;
		} else {
			ni = N - i;
			jc = i;
		}

		// Apply H or H^H to C(ic:ic+mi, jc:jc+ni)
		// zlarfb expects complex-element strides
		zlarfb(
			side, trans, 'Forward', 'Columnwise', mi, ni, ib,
			A, strideA1 / 2, strideA2 / 2, offsetA + i * strideA1 + i * strideA2,
			WORK, 1, ldt, iwt * 2,
			C, strideC1 / 2, strideC2 / 2, offsetC + ic * strideC1 + jc * strideC2,
			WORK, 1, ldwork, 0
		);
	}

	return 0;
}


// EXPORTS //

module.exports = zunmqr;
