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

var zgeqrf = require( '../../zgeqrf/lib/base.js' );
var zlaqp2 = require( '../../zlaqp2/lib/base.js' );
var zlaqps = require( '../../zlaqps/lib/base.js' );
var zswap = require( '../../../../blas/base/zswap/lib/base.js' );
var zunmqr = require( '../../zunmqr/lib/base.js' );
var dznrm2 = require( '../../../../blas/base/dznrm2/lib/base.js' );


// VARIABLES //

var DEFAULT_NB = 32;


// MAIN //

/**
* Computes a QR factorization with column pivoting of an M-by-N matrix:
*   A * P = Q * R
* using level 3 BLAS.
*
* On exit, A is overwritten with the QR factors (R in the upper triangle,
* reflector vectors below). JPVT records the column permutation:
* column j of A*P was column JPVT(j) of A.
*
* JPVT is 1-based (matching Fortran convention): JPVT(j) in [1..N].
* On entry, if JPVT(j) != 0, column j is permuted to the front of A*P;
* if JPVT(j) = 0, column j is free to be pivoted.
*
* Complex elements are stored as interleaved real/imaginary pairs.
* Strides are in complex-element units.
*
* @private
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input/output matrix (interleaved complex)
* @param {integer} strideA1 - first dim stride of A (complex elements)
* @param {integer} strideA2 - second dim stride of A (complex elements)
* @param {NonNegativeInteger} offsetA - starting Float64 index for A
* @param {Int32Array} JPVT - column permutation (1-based on exit)
* @param {integer} strideJPVT - stride for JPVT
* @param {NonNegativeInteger} offsetJPVT - starting index for JPVT
* @param {Float64Array} TAU - output reflector scalars (interleaved complex)
* @param {integer} strideTAU - stride for TAU (complex elements)
* @param {NonNegativeInteger} offsetTAU - starting Float64 index for TAU
* @param {Float64Array} WORK - workspace (interleaved complex)
* @param {integer} strideWORK - stride for WORK (complex elements)
* @param {NonNegativeInteger} offsetWORK - starting Float64 index for WORK
* @param {integer} lwork - workspace size in complex elements (unused)
* @param {Float64Array} RWORK - real workspace (length >= 2*N)
* @param {integer} strideRWORK - stride for RWORK
* @param {NonNegativeInteger} offsetRWORK - starting index for RWORK
* @returns {integer} info - 0 if successful
*/
function zgeqp3( M, N, A, strideA1, strideA2, offsetA, JPVT, strideJPVT, offsetJPVT, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK, lwork, RWORK, strideRWORK, offsetRWORK ) { // eslint-disable-line max-len, max-params
	var topbmn;
	var sminmn;
	var minmn;
	var nbmin;
	var nfxd;
	var sa1;
	var sa2;
	var oA;
	var oJ;
	var oR;
	var oT;
	var fjb;
	var sm;
	var sn;
	var na;
	var nb;
	var nx;
	var j;

	sa1 = strideA1;
	sa2 = strideA2;
	oA = offsetA;
	oJ = offsetJPVT;
	oR = offsetRWORK;
	oT = offsetTAU;

	minmn = Math.min( M, N );
	if ( minmn === 0 ) {
		return 0;
	}

	// Phase 1: Move fixed columns to the front
	nfxd = 0;
	for ( j = 0; j < N; j++ ) {
		if ( JPVT[ oJ + j * strideJPVT ] !== 0 ) {
			if ( j !== nfxd ) {
				// Swap columns j and nfxd
				zswap( M, A, sa1, oA + 2 * j * sa2, A, sa1, oA + 2 * nfxd * sa2 );
				JPVT[ oJ + j * strideJPVT ] = JPVT[ oJ + nfxd * strideJPVT ];
				JPVT[ oJ + nfxd * strideJPVT ] = j + 1; // 1-based
			} else {
				JPVT[ oJ + j * strideJPVT ] = j + 1; // 1-based
			}
			nfxd += 1;
		} else {
			JPVT[ oJ + j * strideJPVT ] = j + 1; // 1-based
		}
	}

	// Phase 2: Factor fixed columns using standard QR
	if ( nfxd > 0 ) {
		na = Math.min( M, nfxd );

		// Allocate internal workspace for zgeqrf
		var workQR = new Float64Array( 2 * Math.max( N, 1 ) * DEFAULT_NB ); // eslint-disable-line no-var

		zgeqrf( M, na, A, sa1, sa2, oA, TAU, strideTAU, oT, workQR, 1, 0 );

		if ( na < N ) {
			// Apply Q^H to remaining columns
			zunmqr(
				'Left', 'Conjugate Transpose', M, N - na, na,
				A, sa1, sa2, oA,
				TAU, strideTAU, oT,
				A, sa1, sa2, oA + 2 * na * sa2,
				workQR, 1, 0, workQR.length / 2
			);
		}
	}

	// Phase 3: Factor the free columns
	if ( nfxd < minmn ) {
		sm = M - nfxd;
		sn = N - nfxd;
		sminmn = minmn - nfxd;

		// Compute initial column norms for the unfactored submatrix
		// RWORK(j) = VN1(j), RWORK(N+j) = VN2(j) for j = nfxd..N-1
		// But we use them starting from index 0 in the subproblem
		for ( j = nfxd; j < N; j++ ) {
			RWORK[ oR + j * strideRWORK ] = dznrm2(
				sm, A, sa1, oA + 2 * ( nfxd * sa1 + j * sa2 )
			);
			RWORK[ oR + ( N + j ) * strideRWORK ] = RWORK[ oR + j * strideRWORK ];
		}

		nb = DEFAULT_NB;
		nbmin = 2;
		nx = 0;

		if ( nb > 1 && nb < sminmn ) {
			nx = 0; // crossover point
			if ( nx < sminmn ) {
				// Check if workspace is sufficient for blocked code
				// If not, reduce block size
			}
		}

		if ( nb >= nbmin && nb < sminmn && nx < sminmn ) {
			// Use blocked code
			j = nfxd;
			topbmn = minmn - nx;

			// Allocate F matrix and AUXV for blocked panel
			var F = new Float64Array( 2 * ( sn + 1 ) * nb ); // eslint-disable-line no-var
			var AUXV = new Float64Array( 2 * nb ); // eslint-disable-line no-var

			while ( j < topbmn ) {
				var jb = Math.min( nb, topbmn - j ); // eslint-disable-line no-var

				// Factor panel using zlaqps
				// zlaqps( M, N-j, j, jb, A(:,j), JPVT(j), TAU(j), RWORK(j), RWORK(N+j), WORK, F, sn+1 )
				fjb = zlaqps(
					M, N - j, j, jb,
					A, sa1, sa2, oA + 2 * j * sa2,
					JPVT, strideJPVT, oJ + j * strideJPVT,
					TAU, strideTAU, oT + 2 * j * strideTAU,
					RWORK, strideRWORK, oR + j * strideRWORK,
					RWORK, strideRWORK, oR + ( N + j ) * strideRWORK,
					AUXV, 1, 0,
					F, 1, sn + 1, 0
				);
				j += fjb;
			}
		} else {
			j = nfxd;
		}

		// Use unblocked code for the remainder
		if ( j < minmn ) {
			zlaqp2(
				M, N - j, j,
				A, sa1, sa2, oA + 2 * j * sa2,
				JPVT, strideJPVT, oJ + j * strideJPVT,
				TAU, strideTAU, oT + 2 * j * strideTAU,
				RWORK, strideRWORK, oR + j * strideRWORK,
				RWORK, strideRWORK, oR + ( N + j ) * strideRWORK,
				WORK, strideWORK, offsetWORK
			);
		}
	}

	return 0;
}


// EXPORTS //

module.exports = zgeqp3;
