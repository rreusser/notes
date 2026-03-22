'use strict';

// MODULES //

var Complex128 = require( '@stdlib/complex/float64/ctor' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zunm2r = require( '../../zunm2r/lib/base.js' );
var zlaset = require( '../../zlaset/lib/base.js' );

// VARIABLES //

var CZERO = new Complex128( 0.0, 0.0 );
var CONE = new Complex128( 1.0, 0.0 );

// MAIN //

/**
* Generate an M-by-N complex matrix Q with orthonormal columns,
* which is defined as the first N columns of a product of K elementary
* reflectors of order M: Q = H(1)*H(2)*...*H(k).
*
* Uses zunm2r to apply reflectors to an identity matrix.
*
* A, TAU, WORK are Complex128Arrays. Strides and offsets are in complex elements.
*
* @private
* @param {NonNegativeInteger} M - rows
* @param {NonNegativeInteger} N - columns
* @param {NonNegativeInteger} K - number of reflectors
* @param {Complex128Array} A - input/output matrix
* @param {integer} strideA1 - first dim stride of A (complex elements)
* @param {integer} strideA2 - second dim stride of A (complex elements)
* @param {NonNegativeInteger} offsetA - starting index for A (complex elements)
* @param {Complex128Array} TAU - scalar factors
* @param {integer} strideTAU - stride for TAU (complex elements)
* @param {NonNegativeInteger} offsetTAU - starting index for TAU
* @param {Complex128Array} WORK - workspace
* @param {integer} lwork - workspace size in complex elements
* @returns {integer} 0 on success
*/
function zungqr( M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, lwork ) { // eslint-disable-line max-len, max-params
	var reflectors;
	var refl_v;
	var Av;
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

	// Get Float64 view for element access
	Av = reinterpret( A, 0 );

	// Save the reflector vectors (lower triangular of A) before overwriting A
	// with the identity matrix. The reflectors are in columns 0..K-1, rows i+1..M-1.
	reflectors = new Complex128Array( M * K );
	refl_v = reinterpret( reflectors, 0 );
	for ( j = 0; j < K; j++ ) {
		for ( i = 0; i < M; i++ ) {
			idx = ( oA + i * sa1 + j * sa2 ) * 2;
			refl_v[ 2 * ( i + j * M ) ] = Av[ idx ];
			refl_v[ 2 * ( i + j * M ) + 1 ] = Av[ idx + 1 ];
		}
	}

	// Set A to identity
	zlaset( 'Full', M, N, CZERO, CONE, A, sa1, sa2, oA );

	// Apply Q = H(1)*H(2)*...*H(K) to identity from the left
	// zunm2r expects strides in complex elements
	zunm2r( 'L', 'N', M, N, K,
		reflectors, 1, M, 0,
		TAU, strideTAU, offsetTAU,
		A, sa1, sa2, oA,
		WORK, 1, 0
	);

	return 0;
}


// EXPORTS //

module.exports = zungqr;
