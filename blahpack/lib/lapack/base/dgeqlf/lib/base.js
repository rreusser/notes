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

/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var Float64Array = require( '@stdlib/array/float64' );
var dgeql2 = require( '../../dgeql2/lib/base.js' );
var dlarfb = require( '../../dlarfb/lib/base.js' );
var dlarft = require( '../../dlarft/lib/base.js' );


// VARIABLES //

var DEFAULT_NB = 32;
var DEFAULT_NX = 128;


// MAIN //

/**
* Computes a QL factorization of a real M-by-N matrix A = Q * L.
* using blocked Householder reflections.
*
* @private
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix (column-major)
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} TAU - output array of scalar factors
* @param {integer} strideTAU - stride length for `TAU`
* @param {NonNegativeInteger} offsetTAU - starting index for `TAU`
* @param {Float64Array} WORK - workspace array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @param {integer} lwork - length of the provided workspace (ignored; internal workspace allocated)
* @returns {integer} status code (0 = success)
*/
function dgeqlf( M, N, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK, lwork ) { // eslint-disable-line no-unused-vars
	var ldwork;
	var nbmin;
	var panel;
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

	// Quick return if possible
	if ( K === 0 ) {
		return 0;
	}

	nb = DEFAULT_NB;
	nbmin = 2;
	nx = DEFAULT_NX;
	iws = N;
	ldwork = N;

	if ( nb > 1 && nb < K ) {
		if ( nx < K ) {
			iws = ldwork * nb;
		}
	}

	// Allocate the T matrix for block reflectors (NB x NB)
	T = new Float64Array( nb * nb );

	// Ensure WORK is large enough for dlarfb (N * NB) and dgeql2 (N)
	if ( !WORK || WORK.length < iws ) {
		WORK = new Float64Array( iws );
		offsetWORK = 0;
		strideWORK = 1;
	}

	if ( nb >= nbmin && nb < K && nx < K ) {
		// Use blocked code initially. The last KK columns are handled by
		// The unblocked cleanup at the end.
		ki = ( Math.floor( ( K - nx - 1 ) / nb ) ) * nb;
		kk = Math.min( K, ki + nb );

		// Fortran: DO I = K-KK+KI+1, K-KK+1, -NB (1-based)

		// 0-based: i from K-KK+ki down to K-KK, step -nb
		for ( i = ( K - kk ) + ki; i >= ( K - kk ); i -= nb ) {
			ib = Math.min( K - i, nb );

			// Panel top-left (0-based): A(0, N-K+i), with "effective M" = M-K+i+ib rows
			panel = offsetA + ( ( ( N - K ) + i ) * strideA2 );

			// Compute QL factorization of the current panel
			dgeql2(( M - K ) + i + ib, ib, A, strideA1, strideA2, panel, TAU, strideTAU, offsetTAU + ( i * strideTAU ), WORK, strideWORK, offsetWORK);

			if ( ( N - K ) + i > 0 ) {
				// Form the triangular factor of the block reflector
				// H = H(i+ib-1) ... H(i+1) H(i)
				dlarft('backward', 'columnwise', ( M - K ) + i + ib, ib, A, strideA1, strideA2, panel, TAU, strideTAU, offsetTAU + ( i * strideTAU ), T, 1, nb, 0);

				// Apply H**T to A(0:M-K+i+ib-1, 0:N-K+i-1) from the left
				dlarfb('left', 'transpose', 'backward', 'columnwise', ( M - K ) + i + ib, ( N - K ) + i, ib, A, strideA1, strideA2, panel, T, 1, nb, 0, A, strideA1, strideA2, offsetA, WORK, 1, ldwork, offsetWORK);
			}
		}
		mu = M - kk;
		nu = N - kk;
	} else {
		mu = M;
		nu = N;
	}

	// Use unblocked code to factor the last or only block
	if ( mu > 0 && nu > 0 ) {
		dgeql2(mu, nu, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK);
	}

	return 0;
}


// EXPORTS //

module.exports = dgeqlf;
