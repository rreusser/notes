'use strict';

// MODULES //

var zlarf = require( '../../../../lapack/base/zlarf/lib/base.js' );

// MAIN //

/**
* Apply a complex elementary reflector Q (stored as products of
* Householder reflectors) to a complex M-by-N matrix C from the left
* or right: C := Q*C, C := Q^H*C, C := C*Q, or C := C*Q^H.
*
* Unblocked algorithm (ZUNM2R). Q = H(1)*H(2)*...*H(k).
*
* Complex elements are stored as interleaved real/imaginary pairs.
* Matrix strides are in doubles (not complex elements).
* Vector strides (for TAU) are in complex elements.
*
* @private
* @param {string} side - 'L' or 'R'
* @param {string} trans - 'N' or 'C'
* @param {NonNegativeInteger} M - rows of C
* @param {NonNegativeInteger} N - columns of C
* @param {NonNegativeInteger} K - number of reflectors
* @param {Float64Array} A - reflector vectors (interleaved complex, column-major)
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
* @param {Float64Array} WORK - workspace (interleaved complex)
* @returns {integer} 0 on success
*/
function zunm2r( side, trans, M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, C, strideC1, strideC2, offsetC, WORK ) { // eslint-disable-line max-len, max-params
	var notran;
	var left;
	var taui;
	var idxA;
	var aii0;
	var aii1;
	var mi;
	var ni;
	var ic;
	var jc;
	var i1;
	var i2;
	var i3;
	var i;

	if ( M === 0 || N === 0 || K === 0 ) {
		return 0;
	}

	left = ( side === 'L' || side === 'l' );
	notran = ( trans === 'N' || trans === 'n' );

	// Determine iteration direction
	if ( ( left && !notran ) || ( !left && notran ) ) {
		i1 = 0;
		i2 = K;
		i3 = 1;
	} else {
		i1 = K - 1;
		i2 = -1;
		i3 = -1;
	}

	if ( left ) {
		ni = N;
		jc = 0;
	} else {
		mi = M;
		ic = 0;
	}

	// Temporary 2-element array for tau
	taui = new Float64Array( 2 );

	for ( i = i1; i !== i2; i += i3 ) {
		if ( left ) {
			mi = M - i;
			ic = i;
		} else {
			ni = N - i;
			jc = i;
		}

		// Get tau_i, conjugating if needed
		if ( notran ) {
			taui[ 0 ] = TAU[ offsetTAU + i * strideTAU * 2 ];
			taui[ 1 ] = TAU[ offsetTAU + i * strideTAU * 2 + 1 ];
		} else {
			taui[ 0 ] = TAU[ offsetTAU + i * strideTAU * 2 ];
			taui[ 1 ] = -TAU[ offsetTAU + i * strideTAU * 2 + 1 ];
		}

		// Save A(i,i) and set it to 1
		idxA = offsetA + i * strideA1 + i * strideA2;
		aii0 = A[ idxA ];
		aii1 = A[ idxA + 1 ];
		A[ idxA ] = 1.0;
		A[ idxA + 1 ] = 0.0;

		// Apply H(i) to C(ic:ic+mi, jc:jc+ni) from the left or right
		// zlarf expects strides in complex elements
		zlarf(
			side, mi, ni,
			A, strideA1 / 2, offsetA + i * strideA1 + i * strideA2,
			taui, 0,
			C, strideC1 / 2, strideC2 / 2, offsetC + ic * strideC1 + jc * strideC2,
			WORK, 1, 0
		);

		// Restore A(i,i)
		A[ idxA ] = aii0;
		A[ idxA + 1 ] = aii1;
	}

	return 0;
}


// EXPORTS //

module.exports = zunm2r;
