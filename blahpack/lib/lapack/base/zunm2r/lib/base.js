/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

'use strict';

// MODULES //

var zlarf = require( '../../zlarf/lib/base.js' );


// MAIN //

/**
* Overwrites the M-by-N matrix C with Q*C, Q^H*C, C*Q, or C*Q^H,
* where Q is a complex unitary matrix defined as the product of K
* elementary reflectors Q = H(1) * H(2) * ... * H(k) as returned
* by ZGEQR2.
*
* Complex elements are stored as interleaved real/imaginary pairs.
* Strides are in complex-element units.
*
* @private
* @param {string} side - 'L' to apply Q from left, 'R' from right
* @param {string} trans - 'N' for Q, 'C' for Q^H
* @param {NonNegativeInteger} M - number of rows of C
* @param {NonNegativeInteger} N - number of columns of C
* @param {NonNegativeInteger} K - number of elementary reflectors
* @param {Float64Array} A - reflector vectors from ZGEQR2 (interleaved complex)
* @param {integer} strideA1 - stride of the first dimension of A (complex elements)
* @param {integer} strideA2 - stride of the second dimension of A (complex elements)
* @param {NonNegativeInteger} offsetA - starting index for A (Float64 index)
* @param {Float64Array} TAU - scalar factors of reflectors (interleaved complex)
* @param {integer} strideTAU - stride for TAU (complex elements)
* @param {NonNegativeInteger} offsetTAU - starting index for TAU (Float64 index)
* @param {Float64Array} C - input/output matrix (interleaved complex)
* @param {integer} strideC1 - stride of the first dimension of C (complex elements)
* @param {integer} strideC2 - stride of the second dimension of C (complex elements)
* @param {NonNegativeInteger} offsetC - starting index for C (Float64 index)
* @param {Float64Array} WORK - workspace (interleaved complex)
* @param {integer} strideWORK - stride for WORK (complex elements)
* @param {NonNegativeInteger} offsetWORK - starting index for WORK (Float64 index)
* @returns {integer} info - 0 if successful
*/
function zunm2r( side, trans, M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
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

		// Get tau_i, conjugating if trans='C'
		if ( notran ) {
			taui[ 0 ] = TAU[ offsetTAU + i * strideTAU * 2 ];
			taui[ 1 ] = TAU[ offsetTAU + i * strideTAU * 2 + 1 ];
		} else {
			taui[ 0 ] = TAU[ offsetTAU + i * strideTAU * 2 ];
			taui[ 1 ] = -TAU[ offsetTAU + i * strideTAU * 2 + 1 ];
		}

		// Save A(i,i) and set it to 1
		idxA = offsetA + 2 * ( i * strideA1 + i * strideA2 );
		aii0 = A[ idxA ];
		aii1 = A[ idxA + 1 ];
		A[ idxA ] = 1.0;
		A[ idxA + 1 ] = 0.0;

		// Apply H(i) to C(ic:ic+mi, jc:jc+ni) from the left or right
		// zlarf expects strides in complex elements
		zlarf(
			side, mi, ni,
			A, strideA1, offsetA + 2 * ( i * strideA1 + i * strideA2 ),
			taui, 0,
			C, strideC1, strideC2, offsetC + 2 * ( ic * strideC1 + jc * strideC2 ),
			WORK, strideWORK, offsetWORK
		);

		// Restore A(i,i)
		A[ idxA ] = aii0;
		A[ idxA + 1 ] = aii1;
	}

	return 0;
}


// EXPORTS //

module.exports = zunm2r;
