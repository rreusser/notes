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

var zgelq2 = require( '../../zgelq2/lib/base.js' );
var zlarfb = require( '../../zlarfb/lib/base.js' );
var zlarft = require( '../../zlarft/lib/base.js' );


// VARIABLES //

var DEFAULT_NB = 32;


// MAIN //

/**
* Computes an LQ factorization of a complex M-by-N matrix A = L * Q
* using blocked Householder reflections.
*
* @private
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix (interleaved complex, column-major)
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} TAU - output array of scalar factors (interleaved complex)
* @param {integer} strideTAU - stride length for `TAU`
* @param {NonNegativeInteger} offsetTAU - starting index for `TAU`
* @param {Float64Array} WORK - workspace array (interleaved complex)
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @returns {integer} status code (0 = success)
*/
function zgelqf( M, N, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	var ldwork;
	var nbmin;
	var sa1;
	var sa2;
	var iws;
	var ib;
	var nb;
	var nx;
	var T;
	var K;
	var i;

	sa1 = strideA1;
	sa2 = strideA2;
	K = Math.min( M, N );

	// Quick return if possible
	if ( K === 0 ) {
		return 0;
	}

	nb = DEFAULT_NB;
	nbmin = 2;
	nx = 0;
	iws = M;

	if ( nb > 1 && nb < K ) {
		// Determine crossover point NX (below which unblocked is faster)
		nx = 0;
		if ( nx < K ) {
			ldwork = M;
			iws = ldwork * nb;
		}
	}

	// Allocate the T matrix for block reflectors (NB x NB, interleaved complex)
	T = new Float64Array( 2 * nb * nb );

	// Ensure WORK is large enough; if not provided or too small, allocate internally
	if ( !WORK || WORK.length < 2 * iws ) {
		WORK = new Float64Array( 2 * iws );
		offsetWORK = 0;
		strideWORK = 1;
	}
	ldwork = M;

	if ( nb >= nbmin && nb < K && nx < K ) {
		// Use blocked code
		i = 0;
		while ( i <= K - 1 - nx ) {
			ib = Math.min( K - i, nb );

			// Compute the LQ factorization of the current panel A(i:i+ib-1, i:N-1)
			zgelq2(
				ib, N - i,
				A, sa1, sa2, offsetA + 2 * ( i * sa1 + i * sa2 ),
				TAU, strideTAU, offsetTAU + 2 * i * strideTAU,
				WORK, strideWORK, offsetWORK
			);

			if ( i + ib < M ) {
				// Form the triangular factor of the block reflector
				// H = H(i) H(i+1) ... H(i+ib-1)
				zlarft(
					'F', 'R',
					N - i, ib,
					A, sa1, sa2, offsetA + 2 * ( i * sa1 + i * sa2 ),
					TAU, strideTAU, offsetTAU + 2 * i * strideTAU,
					T, 1, nb, 0
				);

				// Apply H to A(i+ib:M-1, i:N-1) from the right
				zlarfb(
					'R', 'N', 'F', 'R',
					M - i - ib, N - i, ib,
					A, sa1, sa2, offsetA + 2 * ( i * sa1 + i * sa2 ),
					T, 1, nb, 0,
					A, sa1, sa2, offsetA + 2 * ( ( i + ib ) * sa1 + i * sa2 ),
					WORK, 1, ldwork, offsetWORK
				);
			}
			i += nb;
		}
	} else {
		i = 0;
	}

	// Use unblocked code to factor the last or only block
	if ( i <= K - 1 ) {
		zgelq2(
			M - i, N - i,
			A, sa1, sa2, offsetA + 2 * ( i * sa1 + i * sa2 ),
			TAU, strideTAU, offsetTAU + 2 * i * strideTAU,
			WORK, strideWORK, offsetWORK
		);
	}

	return 0;
}


// EXPORTS //

module.exports = zgelqf;
