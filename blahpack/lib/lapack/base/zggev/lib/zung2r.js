'use strict';

// MODULES //

var zlarf = require( '../../../../lapack/base/zlarf/lib/base.js' );
var zscal = require( '../../../../blas/base/zscal/lib/base.js' );

// MAIN //

/**
* Generate an M-by-N complex matrix Q with orthonormal columns from
* the Householder reflectors stored in the first K columns of A.
*
* Q = H(1)*H(2)*...*H(k), where H(i) = I - tau(i) * v(i) * v(i)^H.
*
* Unblocked algorithm (ZUNG2R).
*
* Complex elements are stored as interleaved real/imaginary pairs.
* Matrix strides are in doubles.
*
* @private
* @param {NonNegativeInteger} M - rows of Q
* @param {NonNegativeInteger} N - columns of Q
* @param {NonNegativeInteger} K - number of reflectors
* @param {Float64Array} A - input/output matrix (interleaved complex)
* @param {integer} strideA1 - first dim stride of A (doubles)
* @param {integer} strideA2 - second dim stride of A (doubles)
* @param {NonNegativeInteger} offsetA - starting index for A
* @param {Float64Array} TAU - scalar factors (interleaved complex)
* @param {integer} strideTAU - stride for TAU (complex elements)
* @param {NonNegativeInteger} offsetTAU - starting index for TAU
* @param {Float64Array} WORK - workspace (interleaved complex)
* @returns {integer} 0 on success
*/
function zung2r( M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK ) { // eslint-disable-line max-len, max-params
	var negTau;
	var idx;
	var sa1;
	var sa2;
	var oA;
	var i;
	var j;
	var l;

	if ( N <= 0 ) {
		return 0;
	}

	sa1 = strideA1;
	sa2 = strideA2;
	oA = offsetA;

	// Initialize columns K+1..N to columns of the unit matrix
	for ( j = K; j < N; j++ ) {
		for ( l = 0; l < M; l++ ) {
			idx = oA + l * sa1 + j * sa2;
			A[ idx ] = 0.0;
			A[ idx + 1 ] = 0.0;
		}
		idx = oA + j * sa1 + j * sa2;
		A[ idx ] = 1.0;
		A[ idx + 1 ] = 0.0;
	}

	negTau = new Float64Array( 2 );

	for ( i = K - 1; i >= 0; i-- ) {
		// Apply H(i) to A(i:M, i+1:N) from the left
		if ( i < N - 1 ) {
			idx = oA + i * sa1 + i * sa2;
			A[ idx ] = 1.0;
			A[ idx + 1 ] = 0.0;

			// zlarf expects complex-element strides
			zlarf(
				'Left', M - i, N - i - 1,
				A, sa1 / 2, oA + i * sa1 + i * sa2,
				TAU, offsetTAU + i * strideTAU * 2,
				A, sa1 / 2, sa2 / 2, oA + i * sa1 + ( i + 1 ) * sa2,
				WORK, 1, 0
			);
		}

		// Scale column i below the diagonal by -tau(i)
		if ( i < M - 1 ) {
			negTau[ 0 ] = -TAU[ offsetTAU + i * strideTAU * 2 ];
			negTau[ 1 ] = -TAU[ offsetTAU + i * strideTAU * 2 + 1 ];
			zscal( M - i - 1, negTau, A, sa1 / 2, oA + ( i + 1 ) * sa1 + i * sa2 );
		}

		// Set A(i,i) = 1 - tau(i)
		idx = oA + i * sa1 + i * sa2;
		A[ idx ] = 1.0 - TAU[ offsetTAU + i * strideTAU * 2 ];
		A[ idx + 1 ] = -TAU[ offsetTAU + i * strideTAU * 2 + 1 ];

		// Set A(1:i-1, i) = 0
		for ( l = 0; l < i; l++ ) {
			idx = oA + l * sa1 + i * sa2;
			A[ idx ] = 0.0;
			A[ idx + 1 ] = 0.0;
		}
	}

	return 0;
}


// EXPORTS //

module.exports = zung2r;
