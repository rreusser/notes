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

/* eslint-disable max-len, max-params, no-var, no-unused-vars */

'use strict';

// MODULES //

var Complex128Array = require( '@stdlib/array/complex128' );
var zgeql2 = require( '../../zgeql2/lib/base.js' );
var zlarfb = require( '../../zlarfb/lib/base.js' );
var zlarft = require( '../../zlarft/lib/base.js' );


// VARIABLES //

var DEFAULT_NB = 32;


// MAIN //

/**
* Computes a QL factorization of a complex M-by-N matrix A = Q * L using blocked Householder reflections.
*
* @private
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Complex128Array} A - input matrix (column-major)
* @param {integer} strideA1 - stride of the first dimension of `A` (in complex elements)
* @param {integer} strideA2 - stride of the second dimension of `A` (in complex elements)
* @param {NonNegativeInteger} offsetA - starting index for `A` (in complex elements)
* @param {Complex128Array} TAU - output array of scalar factors
* @param {integer} strideTAU - stride length for `TAU` (in complex elements)
* @param {NonNegativeInteger} offsetTAU - starting index for `TAU` (in complex elements)
* @param {Complex128Array} WORK - workspace array (or null to auto-allocate)
* @param {integer} strideWORK - stride length for `WORK` (in complex elements)
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK` (in complex elements)
* @param {integer} lwork - workspace length (unused; workspace auto-sized)
* @returns {integer} status code (0 = success)
*/
function zgeqlf( M, N, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK, lwork ) {
	var offsetT;
	var ldwork;
	var nbmin;
	var iws;
	var ib;
	var nb;
	var nx;
	var mu;
	var nu;
	var ki;
	var kk;
	var T;
	var K;
	var i;

	K = Math.min( M, N );
	if ( K === 0 ) {
		return 0;
	}

	nb = DEFAULT_NB;
	nbmin = 2;
	nx = 0;
	iws = N;
	ldwork = N;

	if ( WORK === null ) {
		WORK = new Complex128Array( Math.max( 1, ( N * nb ) + ( nb * nb ) ) );
	}
	T = WORK;

	if ( nb > 1 && nb < K ) {
		iws = ldwork * nb;
	}
	offsetT = offsetWORK + iws;

	if ( nb >= nbmin && nb < K && nx < K ) {
		// Use blocked code initially (Fortran: KI = ((K-NX-1)/NB)*NB; KK = MIN(K, KI+NB))
		ki = Math.floor( ( K - nx - 1 ) / nb ) * nb;
		kk = Math.min( K, ki + nb );

		// Fortran DO 10 I = K-KK+KI+1, K-KK+1, -NB (1-based); 0-based: i starts at K-kk+ki, ends (inclusive) at K-kk, step -nb
		i = K - kk + ki;
		while ( i >= K - kk ) {
			ib = Math.min( K - i, nb );

			// Compute the QL factorization of the current block A( 0:M-K+i+ib-1, N-K+i:N-K+i+ib-1 ); panel has (M-K+i+ib) rows and ib columns
			zgeql2( M - K + i + ib, ib, A, strideA1, strideA2, offsetA + ( ( N - K + i ) * strideA2 ), TAU, strideTAU, offsetTAU + ( i * strideTAU ), WORK, strideWORK, offsetWORK );

			if ( N - K + i > 0 ) {
				// Form the triangular factor of the block reflector H = H(i+ib-1) ... H(i+1) H(i) (backward, columnwise)
				zlarft( 'backward', 'columnwise', M - K + i + ib, ib, A, strideA1, strideA2, offsetA + ( ( N - K + i ) * strideA2 ), TAU, strideTAU, offsetTAU + ( i * strideTAU ), T, 1, nb, offsetT );

				// Apply H^H to A( 0:M-K+i+ib-1, 0:N-K+i-1 ) from the left
				zlarfb( 'left', 'conjugate-transpose', 'backward', 'columnwise', M - K + i + ib, N - K + i, ib, A, strideA1, strideA2, offsetA + ( ( N - K + i ) * strideA2 ), T, 1, nb, offsetT, A, strideA1, strideA2, offsetA, WORK, 1, ldwork, offsetWORK );
			}
			i -= nb;
		}

		// Fortran post-loop: I = K-KK+1-NB (1-based), so MU = M-K+I+NB-1 = M-KK and NU = N-KK
		mu = M - kk;
		nu = N - kk;
	} else {
		mu = M;
		nu = N;
	}

	// Use unblocked code to factor the last or only block
	if ( mu > 0 && nu > 0 ) {
		zgeql2( mu, nu, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK );
	}

	return 0;
}


// EXPORTS //

module.exports = zgeqlf;
