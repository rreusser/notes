'use strict';

// MODULES //

var zunm2r = require( '../../zunm2r/lib/base.js' );
var zlaset = require( '../../zlaset/lib/base.js' );

// VARIABLES //

var CZERO = new Float64Array( [ 0.0, 0.0 ] );
var CONE = new Float64Array( [ 1.0, 0.0 ] );

// MAIN //

/**
* Generate an M-by-N complex matrix Q with orthonormal columns,
* which is defined as the first N columns of a product of K elementary
* reflectors of order M: Q = H(1)*H(2)*...*H(k).
*
* Uses zunm2r to apply reflectors to an identity matrix.
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
	var reflectors;
	var sa1;
	var sa2;
	var oA;
	var idx;
	var i;
	var j;

	if ( N <= 0 ) {
		return 0;
	}

	sa1 = strideA1;
	sa2 = strideA2;
	oA = offsetA;

	// Save the reflector vectors (lower triangular of A) before overwriting A
	// with the identity matrix. The reflectors are in columns 0..K-1, rows i+1..M-1.
	reflectors = new Float64Array( 2 * M * K );
	for ( j = 0; j < K; j++ ) {
		for ( i = 0; i < M; i++ ) {
			idx = oA + i * sa1 + j * sa2;
			reflectors[ 2 * ( i + j * M ) ] = A[ idx ];
			reflectors[ 2 * ( i + j * M ) + 1 ] = A[ idx + 1 ];
		}
	}

	// Set A to identity
	zlaset( 'Full', M, N, CZERO, CONE, A, sa1, sa2, oA );

	// Apply Q = H(1)*H(2)*...*H(K) to identity from the left
	// zunm2r('L', 'N', M, N, K, reflectors, 2, 2*M, 0, TAU, strideTAU, offsetTAU, A, sa1, sa2, oA, WORK)
	zunm2r( 'L', 'N', M, N, K,
		reflectors, 2, 2 * M, 0,
		TAU, strideTAU, offsetTAU,
		A, sa1, sa2, oA,
		WORK
	);

	return 0;
}


// EXPORTS //

module.exports = zungqr;
