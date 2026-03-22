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

var zlacgv = require( '../../zlacgv/lib/base.js' );
var zlarfg = require( '../../zlarfg/lib/base.js' );
var zlarf = require( '../../zlarf/lib/base.js' );


// MAIN //

/**
* Computes an LQ factorization of a complex M-by-N matrix A = L * Q
* using Householder reflections (unblocked algorithm).
*
* @private
* @param {NonNegativeInteger} M - number of rows in A
* @param {NonNegativeInteger} N - number of columns in A
* @param {Float64Array} A - input/output matrix (interleaved complex, column-major)
* @param {integer} strideA1 - stride of the first dimension of A
* @param {integer} strideA2 - stride of the second dimension of A
* @param {NonNegativeInteger} offsetA - starting index for A
* @param {Float64Array} TAU - output array of scalar factors (interleaved complex)
* @param {integer} strideTAU - stride for TAU (in complex elements)
* @param {NonNegativeInteger} offsetTAU - starting index for TAU
* @param {Float64Array} WORK - workspace array (interleaved complex, length >= M)
* @param {integer} strideWORK - stride for WORK
* @param {NonNegativeInteger} offsetWORK - starting index for WORK
* @returns {integer} info - 0 if successful
*/
function zgelq2( M, N, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	var alpha_re;
	var alpha_im;
	var tau_off;
	var sa1;
	var sa2;
	var aii;
	var xoff;
	var K;
	var i;

	sa1 = strideA1;
	sa2 = strideA2;
	K = Math.min( M, N );

	for ( i = 0; i < K; i++ ) {
		// Index of A(i,i) in the interleaved array
		aii = offsetA + 2 * ( i * sa1 + i * sa2 );

		// Conjugate the i-th row from column i to N-1
		// In Fortran: ZLACGV( N-I+1, A(I,I), LDA )
		// stride along the row = sa2 (column stride)
		zlacgv( N - i, A, sa2, aii );

		// Index of A(i, min(i+1, N-1)) — the start of the x vector to the right of the diagonal
		xoff = offsetA + 2 * ( i * sa1 + Math.min( i + 1, N - 1 ) * sa2 );

		// tau(i) offset
		tau_off = offsetTAU + 2 * i * strideTAU;

		// Generate elementary reflector H(i) to annihilate A(i, i+1:N-1)
		// zlarfg( N, alpha, offsetAlpha, x, strideX, offsetX, tau, offsetTau )
		// The reflector works along the row, so stride = sa2 (column stride)
		// zlarfg overwrites A(i,i) with beta
		zlarfg( N - i, A, aii, A, sa2, xoff, TAU, tau_off );

		if ( i < M - 1 ) {
			// Save A(i,i) (which now holds beta) and set to 1 for the reflector application
			alpha_re = A[ aii ];
			alpha_im = A[ aii + 1 ];
			A[ aii ] = 1.0;
			A[ aii + 1 ] = 0.0;

			// Apply H(i) to A(i+1:M-1, i:N-1) from the right
			// In Fortran: ZLARF( 'Right', M-I, N-I+1, A(I,I), LDA, TAU(I), A(I+1,I), LDA, WORK )
			// v vector is the i-th row starting at column i, stride = sa2
			// C submatrix starts at A(i+1, i), dimensions (M-i-1) x (N-i)
			zlarf( 'R', M - i - 1, N - i, A, sa2, aii, TAU, tau_off,
				A, sa1, sa2, offsetA + 2 * ( ( i + 1 ) * sa1 + i * sa2 ),
				WORK, strideWORK, offsetWORK );

			// Restore A(i,i)
			A[ aii ] = alpha_re;
			A[ aii + 1 ] = alpha_im;
		}

		// Unconjugate the i-th row from column i to N-1
		// In Fortran: ZLACGV( N-I+1, A(I,I), LDA )
		zlacgv( N - i, A, sa2, aii );
	}
	return 0;
}


// EXPORTS //

module.exports = zgelq2;
