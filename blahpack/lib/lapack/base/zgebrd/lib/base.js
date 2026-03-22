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

var zgebd2 = require( '../../zgebd2/lib/base.js' );
var zgemm = require( '../../../../blas/base/zgemm/lib/base.js' );
var zlabrd = require( '../../zlabrd/lib/base.js' );


// VARIABLES //

var DEFAULT_NB = 32;
var ONE = new Float64Array( [ 1.0, 0.0 ] );
var NEGONE = new Float64Array( [ -1.0, 0.0 ] );


// MAIN //

/**
* Reduces a complex M-by-N matrix A to upper or lower real bidiagonal form B
* by a unitary transformation: Q^H * A * P = B.
*
* This is the blocked version that processes NB columns at a time using
* zlabrd for the panel factorization, zgemm for the trailing matrix update,
* and zgebd2 for the final unblocked piece.
*
* If M >= N, B is upper bidiagonal; if M < N, B is lower bidiagonal.
*
* @private
* @param {NonNegativeInteger} M - number of rows of A
* @param {NonNegativeInteger} N - number of columns of A
* @param {Float64Array} A - input/output matrix (interleaved complex, column-major)
* @param {integer} strideA1 - stride of the first dimension of A (in complex elements)
* @param {integer} strideA2 - stride of the second dimension of A (in complex elements)
* @param {NonNegativeInteger} offsetA - starting index for A (Float64 index)
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
* @param {Float64Array} WORK - workspace array (interleaved complex)
* @param {integer} strideWORK - stride for WORK (in complex elements)
* @param {NonNegativeInteger} offsetWORK - starting index for WORK
* @param {integer} lwork - length of WORK array (in complex elements)
* @returns {integer} info - 0 if successful
*/
function zgebrd( M, N, A, strideA1, strideA2, offsetA, d, strideD, offsetD, e, strideE, offsetE, TAUQ, strideTAUQ, offsetTAUQ, TAUP, strideTAUP, offsetTAUP, WORK, strideWORK, offsetWORK, lwork ) { // eslint-disable-line max-len, max-params
	var ldwrkx;
	var ldwrky;
	var minmn;
	var nbmin;
	var aii;
	var aij;
	var sa1;
	var sa2;
	var nb;
	var nx;
	var ws;
	var i;
	var j;

	sa1 = strideA1;
	sa2 = strideA2;
	minmn = Math.min( M, N );

	// Quick return if possible
	if ( minmn === 0 ) {
		return 0;
	}

	// Determine the block size
	nb = DEFAULT_NB;
	ldwrkx = M;
	ldwrky = N;
	ws = Math.max( M, N );
	nx = minmn;

	if ( nb > 1 && nb < minmn ) {
		// Determine crossover point NX (below which unblocked is faster)
		nx = Math.max( nb, 0 );

		if ( nx < minmn ) {
			ws = ( M + N ) * nb;
			if ( lwork < ws ) {
				// Not enough workspace for optimal NB; determine minimum NB
				nbmin = 2;
				if ( lwork >= ( M + N ) * nbmin ) {
					nb = Math.floor( lwork / ( M + N ) );
				} else {
					nb = 1;
					nx = minmn;
				}
			}
		}
	}

	// Ensure WORK is large enough; allocate internally if needed
	if ( !WORK || WORK.length < 2 * ws ) {
		WORK = new Float64Array( 2 * ws );
		offsetWORK = 0;
		strideWORK = 1;
	}

	// Blocked loop: reduce NB columns/rows at a time
	i = 0;
	if ( nb >= 2 && nb < minmn && nx < minmn ) {
		while ( i < minmn - nx ) {
			// Reduce rows and columns i:i+nb-1 to bidiagonal form and return
			// the matrices X and Y which are needed to update the unreduced
			// part of the matrix.
			//
			// zlabrd( M, N, nb, A, sa1, sa2, oA, d, sD, oD, e, sE, oE,
			//         TAUQ, sTQ, oTQ, TAUP, sTP, oTP, X, sX1, sX2, oX, Y, sY1, sY2, oY )
			//
			// Fortran: ZLABRD( M-I+1, N-I+1, NB, A(I,I), LDA, D(I), E(I),
			//                  TAUQ(I), TAUP(I), WORK, LDWRKX,
			//                  WORK(LDWRKX*NB+1), LDWRKY )
			//
			// In Fortran, WORK is used as two 2D matrices:
			//   X = WORK(1..LDWRKX*NB), stored column-major with leading dim LDWRKX
			//   Y = WORK(LDWRKX*NB+1..), stored column-major with leading dim LDWRKY
			//
			// In JS (interleaved complex), X starts at offsetWORK,
			// Y starts at offsetWORK + 2*ldwrkx*nb.
			zlabrd(
				M - i, N - i, nb,
				A, sa1, sa2, offsetA + 2 * ( i * sa1 + i * sa2 ),
				d, strideD, offsetD + i * strideD,
				e, strideE, offsetE + i * strideE,
				TAUQ, strideTAUQ, offsetTAUQ + 2 * i * strideTAUQ,
				TAUP, strideTAUP, offsetTAUP + 2 * i * strideTAUP,
				WORK, 1, ldwrkx, offsetWORK,
				WORK, 1, ldwrky, offsetWORK + 2 * ldwrkx * nb
			);

			// Update the trailing submatrix A(i+nb:M-1, i+nb:N-1)
			// using the matrices X and Y returned by zlabrd.
			//
			// Fortran:
			//   ZGEMM('N', 'C', M-I-NB+1, N-I-NB+1, NB, -ONE,
			//          A(I+NB,I), LDA,
			//          WORK(LDWRKX*NB+NB+1), LDWRKY, ONE,
			//          A(I+NB,I+NB), LDA)
			//
			//   ZGEMM('N', 'N', M-I-NB+1, N-I-NB+1, NB, -ONE,
			//          WORK(NB+1), LDWRKX,
			//          A(I,I+NB), LDA, ONE,
			//          A(I+NB,I+NB), LDA)
			if ( M - i - nb > 0 && N - i - nb > 0 ) {
				// C := -1 * A(i+nb:,i:i+nb-1) * Y(nb:,:)^H + 1 * A(i+nb:,i+nb:)
				zgemm(
					'N', 'C',
					M - i - nb, N - i - nb, nb,
					NEGONE,
					A, sa1, sa2, offsetA + 2 * ( ( i + nb ) * sa1 + i * sa2 ),
					WORK, 1, ldwrky, offsetWORK + 2 * ( ldwrkx * nb + nb ),
					ONE,
					A, sa1, sa2, offsetA + 2 * ( ( i + nb ) * sa1 + ( i + nb ) * sa2 )
				);

				// C := -1 * X(nb:,:) * A(i:i+nb-1,i+nb:) + 1 * A(i+nb:,i+nb:)
				zgemm(
					'N', 'N',
					M - i - nb, N - i - nb, nb,
					NEGONE,
					WORK, 1, ldwrkx, offsetWORK + 2 * nb,
					A, sa1, sa2, offsetA + 2 * ( i * sa1 + ( i + nb ) * sa2 ),
					ONE,
					A, sa1, sa2, offsetA + 2 * ( ( i + nb ) * sa1 + ( i + nb ) * sa2 )
				);
			}

			// Copy diagonal and off-diagonal elements of B back into A
			// (zlabrd modifies A in place for the reflectors, but D and E
			// hold the bidiagonal entries that were overwritten)
			if ( M >= N ) {
				// Upper bidiagonal: D(j) -> A(j,j), E(j) -> A(j,j+1)
				for ( j = i; j < i + nb; j++ ) {
					aii = offsetA + 2 * ( j * sa1 + j * sa2 );
					A[ aii ] = d[ offsetD + j * strideD ];
					A[ aii + 1 ] = 0.0;
					aij = offsetA + 2 * ( j * sa1 + ( j + 1 ) * sa2 );
					A[ aij ] = e[ offsetE + j * strideE ];
					A[ aij + 1 ] = 0.0;
				}
			} else {
				// Lower bidiagonal: D(j) -> A(j,j), E(j) -> A(j+1,j)
				for ( j = i; j < i + nb; j++ ) {
					aii = offsetA + 2 * ( j * sa1 + j * sa2 );
					A[ aii ] = d[ offsetD + j * strideD ];
					A[ aii + 1 ] = 0.0;
					aij = offsetA + 2 * ( ( j + 1 ) * sa1 + j * sa2 );
					A[ aij ] = e[ offsetE + j * strideE ];
					A[ aij + 1 ] = 0.0;
				}
			}

			i += nb;
		}
	}

	// Use unblocked code to factor the last or only block
	zgebd2(
		M - i, N - i,
		A, sa1, sa2, offsetA + 2 * ( i * sa1 + i * sa2 ),
		d, strideD, offsetD + i * strideD,
		e, strideE, offsetE + i * strideE,
		TAUQ, strideTAUQ, offsetTAUQ + 2 * i * strideTAUQ,
		TAUP, strideTAUP, offsetTAUP + 2 * i * strideTAUP,
		WORK, strideWORK, offsetWORK
	);

	return 0;
}


// EXPORTS //

module.exports = zgebrd;
