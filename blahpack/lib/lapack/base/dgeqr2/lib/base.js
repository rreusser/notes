'use strict';

// MODULES //

var dlarfg = require( '../../dlarfg/lib/base.js' );
var dlarf = require( '../../dlarf/lib/base.js' );

// MAIN //

/**
* Computes a QR factorization of a real M-by-N matrix A = Q * R
* using Householder reflections (unblocked algorithm).
*
* @private
* @param {NonNegativeInteger} M - number of rows in A
* @param {NonNegativeInteger} N - number of columns in A
* @param {Float64Array} A - input/output matrix (column-major)
* @param {integer} strideA1 - stride of the first dimension of A
* @param {integer} strideA2 - stride of the second dimension of A
* @param {NonNegativeInteger} offsetA - starting index for A
* @param {Float64Array} TAU - output array of scalar factors
* @param {integer} strideTAU - stride for TAU
* @param {NonNegativeInteger} offsetTAU - starting index for TAU
* @param {Float64Array} WORK - workspace array (length >= N)
* @param {integer} strideWORK - stride for WORK
* @param {NonNegativeInteger} offsetWORK - starting index for WORK
* @returns {integer} info - 0 if successful
*/
function dgeqr2( M, N, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	var aii;
	var K;
	var i;

	K = Math.min( M, N );

	for ( i = 0; i < K; i++ ) {
		// Generate elementary reflector H(i) to annihilate A(i+1:M-1, i)
		aii = offsetA + i * strideA1 + i * strideA2;

		dlarfg( M - i, A, aii,
			A, strideA1, offsetA + Math.min( i + 1, M - 1 ) * strideA1 + i * strideA2,
			TAU, offsetTAU + i * strideTAU );

		if ( i < N - 1 ) {
			// Save A(i,i) and set to 1 for the reflector application
			var alpha = A[ aii ]; // eslint-disable-line no-var
			A[ aii ] = 1.0;

			// Apply H(i) to A(i:M-1, i+1:N-1) from the left
			dlarf( 'left', M - i, N - i - 1, A, strideA1, aii, TAU[ offsetTAU + i * strideTAU ],
				A, strideA1, strideA2, offsetA + i * strideA1 + ( i + 1 ) * strideA2,
				WORK, strideWORK, offsetWORK );

			// Restore A(i,i)
			A[ aii ] = alpha;
		}
	}
	return 0;
}


// EXPORTS //

module.exports = dgeqr2;
