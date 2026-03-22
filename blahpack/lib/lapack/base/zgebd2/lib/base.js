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

var zlarfg = require( '../../zlarfg/lib/base.js' );
var zlarf = require( '../../zlarf/lib/base.js' );
var zlacgv = require( '../../zlacgv/lib/base.js' );


// MAIN //

/**
* Reduces a complex M-by-N matrix A to upper or lower real bidiagonal form B
* by a unitary transformation: Q^H * A * P = B.
*
* If M >= N, B is upper bidiagonal; if M < N, B is lower bidiagonal.
*
* @private
* @param {NonNegativeInteger} M - number of rows of A
* @param {NonNegativeInteger} N - number of columns of A
* @param {Float64Array} A - input/output matrix (interleaved complex, column-major)
* @param {integer} strideA1 - stride of the first dimension of A
* @param {integer} strideA2 - stride of the second dimension of A
* @param {NonNegativeInteger} offsetA - starting index for A
* @param {Float64Array} d - output array of real diagonal elements of B
* @param {integer} strideD - stride for d
* @param {NonNegativeInteger} offsetD - starting index for d
* @param {Float64Array} e - output array of real off-diagonal elements of B
* @param {integer} strideE - stride for e
* @param {NonNegativeInteger} offsetE - starting index for e
* @param {Float64Array} TAUQ - output array of scalar factors for Q (interleaved complex)
* @param {integer} strideTAUQ - stride for TAUQ (in complex elements)
* @param {NonNegativeInteger} offsetTAUQ - starting index for TAUQ
* @param {Float64Array} TAUP - output array of scalar factors for P (interleaved complex)
* @param {integer} strideTAUP - stride for TAUP (in complex elements)
* @param {NonNegativeInteger} offsetTAUP - starting index for TAUP
* @param {Float64Array} WORK - workspace array (interleaved complex, length >= max(M,N))
* @param {integer} strideWORK - stride for WORK (in complex elements)
* @param {NonNegativeInteger} offsetWORK - starting index for WORK
* @returns {integer} info - 0 if successful
*/
function zgebd2( M, N, A, strideA1, strideA2, offsetA, d, strideD, offsetD, e, strideE, offsetE, TAUQ, strideTAUQ, offsetTAUQ, TAUP, strideTAUP, offsetTAUP, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	var conj_tauq;
	var alpha_re;
	var tauq_off;
	var taup_off;
	var xoff;
	var aii;
	var aij;
	var sa1;
	var sa2;
	var i;

	sa1 = strideA1;
	sa2 = strideA2;
	conj_tauq = new Float64Array( 2 );

	if ( M >= N ) {
		// Reduce to upper bidiagonal form
		for ( i = 0; i < N; i++ ) {
			// Index of A(i,i) in interleaved array
			aii = offsetA + 2 * ( i * sa1 + i * sa2 );

			// Index of A(min(i+1, M-1), i) — start of x vector below diagonal
			xoff = offsetA + 2 * ( Math.min( i + 1, M - 1 ) * sa1 + i * sa2 );

			// TAUQ(i) offset
			tauq_off = offsetTAUQ + 2 * i * strideTAUQ;

			// Generate elementary reflector H(i) to annihilate A(i+1:M-1, i)
			// zlarfg( N, alpha, offsetAlpha, x, strideX, offsetX, tau, offsetTau )
			zlarfg( M - i, A, aii, A, sa1, xoff, TAUQ, tauq_off );

			// D(i) = real(alpha)
			d[ offsetD + i * strideD ] = A[ aii ];

			// A(i,i) = 1.0 + 0.0i
			A[ aii ] = 1.0;
			A[ aii + 1 ] = 0.0;

			// Apply H(i)^H to A(i:M-1, i+1:N-1) from the left
			if ( i < N - 1 ) {
				// zlarf uses conj(tau) for left application of H^H
				conj_tauq[ 0 ] = TAUQ[ tauq_off ];
				conj_tauq[ 1 ] = -TAUQ[ tauq_off + 1 ];

				// zlarf( side, M, N, v, strideV, offsetV, tau, offsetTau, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK )
				zlarf( 'L', M - i, N - i - 1, A, sa1, aii, conj_tauq, 0,
					A, sa1, sa2, offsetA + 2 * ( i * sa1 + ( i + 1 ) * sa2 ),
					WORK, strideWORK, offsetWORK );
			}

			// Restore A(i,i) = D(i) (real, so imag = 0)
			A[ aii ] = d[ offsetD + i * strideD ];
			A[ aii + 1 ] = 0.0;

			if ( i < N - 1 ) {
				// TAUP(i) offset
				taup_off = offsetTAUP + 2 * i * strideTAUP;

				// Index of A(i, i+1) in interleaved array
				aij = offsetA + 2 * ( i * sa1 + ( i + 1 ) * sa2 );

				// Conjugate row A(i, i+1:N-1) for the reflector generation
				// zlacgv( N, x, stride, offset )
				zlacgv( N - i - 1, A, sa2, aij );

				// Generate elementary reflector G(i) to annihilate A(i, i+2:N-1)
				// xoff for A(i, min(i+2, N-1))
				xoff = offsetA + 2 * ( i * sa1 + Math.min( i + 2, N - 1 ) * sa2 );
				zlarfg( N - i - 1, A, aij, A, sa2, xoff, TAUP, taup_off );

				// E(i) = real(alpha)
				e[ offsetE + i * strideE ] = A[ aij ];

				// A(i, i+1) = 1.0 + 0.0i
				A[ aij ] = 1.0;
				A[ aij + 1 ] = 0.0;

				// Apply G(i) to A(i+1:M-1, i+1:N-1) from the right
				zlarf( 'R', M - i - 1, N - i - 1, A, sa2, aij, TAUP, taup_off,
					A, sa1, sa2, offsetA + 2 * ( ( i + 1 ) * sa1 + ( i + 1 ) * sa2 ),
					WORK, strideWORK, offsetWORK );

				// Unconjugate row A(i, i+1:N-1)
				zlacgv( N - i - 1, A, sa2, aij );

				// Restore A(i, i+1) = E(i) (real, so imag = 0)
				A[ aij ] = e[ offsetE + i * strideE ];
				A[ aij + 1 ] = 0.0;
			} else {
				// TAUP(i) = 0
				taup_off = offsetTAUP + 2 * i * strideTAUP;
				TAUP[ taup_off ] = 0.0;
				TAUP[ taup_off + 1 ] = 0.0;
			}
		}
	} else {
		// Reduce to lower bidiagonal form
		for ( i = 0; i < M; i++ ) {
			// Index of A(i,i) in interleaved array
			aii = offsetA + 2 * ( i * sa1 + i * sa2 );

			// TAUP(i) offset
			taup_off = offsetTAUP + 2 * i * strideTAUP;

			// Conjugate row A(i, i:N-1) for the reflector generation
			zlacgv( N - i, A, sa2, aii );

			// Generate elementary reflector G(i) to annihilate A(i, i+1:N-1)
			xoff = offsetA + 2 * ( i * sa1 + Math.min( i + 1, N - 1 ) * sa2 );
			zlarfg( N - i, A, aii, A, sa2, xoff, TAUP, taup_off );

			// D(i) = real(alpha)
			d[ offsetD + i * strideD ] = A[ aii ];

			// A(i,i) = 1.0 + 0.0i
			A[ aii ] = 1.0;
			A[ aii + 1 ] = 0.0;

			// Apply G(i) to A(i+1:M-1, i:N-1) from the right
			if ( i < M - 1 ) {
				zlarf( 'R', M - i - 1, N - i, A, sa2, aii, TAUP, taup_off,
					A, sa1, sa2, offsetA + 2 * ( ( i + 1 ) * sa1 + i * sa2 ),
					WORK, strideWORK, offsetWORK );
			}

			// Unconjugate row A(i, i:N-1)
			zlacgv( N - i, A, sa2, aii );

			// Restore A(i,i) = D(i) (real, so imag = 0)
			A[ aii ] = d[ offsetD + i * strideD ];
			A[ aii + 1 ] = 0.0;

			if ( i < M - 1 ) {
				// TAUQ(i) offset
				tauq_off = offsetTAUQ + 2 * i * strideTAUQ;

				// Index of A(i+1, i) in interleaved array
				aij = offsetA + 2 * ( ( i + 1 ) * sa1 + i * sa2 );

				// Generate elementary reflector H(i) to annihilate A(i+2:M-1, i)
				xoff = offsetA + 2 * ( Math.min( i + 2, M - 1 ) * sa1 + i * sa2 );
				zlarfg( M - i - 1, A, aij, A, sa1, xoff, TAUQ, tauq_off );

				// E(i) = real(alpha)
				e[ offsetE + i * strideE ] = A[ aij ];

				// A(i+1, i) = 1.0 + 0.0i
				A[ aij ] = 1.0;
				A[ aij + 1 ] = 0.0;

				// Apply H(i)^H to A(i+1:M-1, i+1:N-1) from the left
				conj_tauq[ 0 ] = TAUQ[ tauq_off ];
				conj_tauq[ 1 ] = -TAUQ[ tauq_off + 1 ];

				zlarf( 'L', M - i - 1, N - i - 1, A, sa1, aij, conj_tauq, 0,
					A, sa1, sa2, offsetA + 2 * ( ( i + 1 ) * sa1 + ( i + 1 ) * sa2 ),
					WORK, strideWORK, offsetWORK );

				// Restore A(i+1, i) = E(i) (real, so imag = 0)
				A[ aij ] = e[ offsetE + i * strideE ];
				A[ aij + 1 ] = 0.0;
			} else {
				// TAUQ(i) = 0
				tauq_off = offsetTAUQ + 2 * i * strideTAUQ;
				TAUQ[ tauq_off ] = 0.0;
				TAUQ[ tauq_off + 1 ] = 0.0;
			}
		}
	}

	return 0;
}


// EXPORTS //

module.exports = zgebd2;
