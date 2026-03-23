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

var daxpy = require( '../../../../blas/base/daxpy/lib/base.js' );
var dcopy = require( '../../../../blas/base/dcopy/lib/base.js' );
var dgemm = require( '../../../../blas/base/dgemm/lib/base.js' );
var dgemv = require( '../../../../blas/base/dgemv/lib/base.js' );
var dlacpy = require( '../../dlacpy/lib/base.js' );
var dlarfg = require( '../../dlarfg/lib/base.js' );
var dscal = require( '../../../../blas/base/dscal/lib/base.js' );
var dtrmm = require( '../../../../blas/base/dtrmm/lib/base.js' );
var dtrmv = require( '../../../../blas/base/dtrmv/lib/base.js' );

// MAIN //

/**
* Reduces the first NB columns of a real general n-by-(n-k+1) matrix A so
* that elements below the k-th subdiagonal are zero. Returns the matrices V
* and T which determine Q as a block reflector I - V*T*V**T, and also Y = A*V*T.
*
* @private
* @param {NonNegativeInteger} N - number of rows of the matrix A
* @param {NonNegativeInteger} K - offset for the reduction
* @param {integer} nb - number of columns to be reduced
* @param {Float64Array} A - input matrix A (n-by-(n-k+1))
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} tau - output array for scalar factors (length nb)
* @param {integer} strideTAU - stride length for `tau`
* @param {NonNegativeInteger} offsetTAU - starting index for `tau`
* @param {Float64Array} T - output upper triangular matrix (nb-by-nb)
* @param {integer} strideT - first-dimension stride for `T`
* @param {NonNegativeInteger} offsetT - starting index for `T`
* @param {integer} ldT - leading (second) dimension stride for `T`
* @param {Float64Array} Y - output matrix (n-by-nb)
* @param {integer} strideY - first-dimension stride for `Y`
* @param {NonNegativeInteger} offsetY - starting index for `Y`
* @param {integer} ldY - leading (second) dimension stride for `Y`
*/
function dlahr2( N, K, nb, A, strideA1, strideA2, offsetA, tau, strideTAU, offsetTAU, T, strideT, offsetT, ldT, Y, strideY, offsetY, ldY ) {
	var ei, i;
	if ( N <= 1 ) { return; }
	ei = 0.0;
	for ( i = 0; i < nb; i++ ) {
		if ( i > 0 ) {
			// Update A(K+1:N, I) — subtract Y * V**T contribution
			dgemv( 'no-transpose', N - K, i, -1.0, Y, strideY, ldY, offsetY + K * strideY, A, strideA2, offsetA + ( K + i - 1 ) * strideA1, 1.0, A, strideA1, offsetA + K * strideA1 + i * strideA2 );

			// Apply I - V * T**T * V**T to column i from the left, using last column of T as workspace
			// w := V1**T * b1
			dcopy( i, A, strideA1, offsetA + K * strideA1 + i * strideA2, T, strideT, offsetT + ( nb - 1 ) * ldT );
			dtrmv( 'lower', 'transpose', 'unit', i, A, strideA1, strideA2, offsetA + K * strideA1, T, strideT, offsetT + ( nb - 1 ) * ldT );

			// w := w + V2**T * b2
			dgemv( 'transpose', N - K - i, i, 1.0, A, strideA1, strideA2, offsetA + ( K + i ) * strideA1, A, strideA1, offsetA + ( K + i ) * strideA1 + i * strideA2, 1.0, T, strideT, offsetT + ( nb - 1 ) * ldT );

			// w := T**T * w
			dtrmv( 'upper', 'transpose', 'non-unit', i, T, strideT, ldT, offsetT, T, strideT, offsetT + ( nb - 1 ) * ldT );

			// b2 := b2 - V2 * w
			dgemv( 'no-transpose', N - K - i, i, -1.0, A, strideA1, strideA2, offsetA + ( K + i ) * strideA1, T, strideT, offsetT + ( nb - 1 ) * ldT, 1.0, A, strideA1, offsetA + ( K + i ) * strideA1 + i * strideA2 );

			// b1 := b1 - V1 * w
			dtrmv( 'lower', 'no-transpose', 'unit', i, A, strideA1, strideA2, offsetA + K * strideA1, T, strideT, offsetT + ( nb - 1 ) * ldT );
			daxpy( i, -1.0, T, strideT, offsetT + ( nb - 1 ) * ldT, A, strideA1, offsetA + K * strideA1 + i * strideA2 );

			// Restore saved subdiagonal element
			A[ offsetA + ( K + i - 1 ) * strideA1 + ( i - 1 ) * strideA2 ] = ei;
		}

		// Generate elementary reflector H(i) to annihilate A(K+I+1:N, I)
		dlarfg( N - K - i, A, offsetA + ( K + i ) * strideA1 + i * strideA2, A, strideA1, offsetA + Math.min( K + i + 1, N - 1 ) * strideA1 + i * strideA2, tau, offsetTAU + i * strideTAU );
		ei = A[ offsetA + ( K + i ) * strideA1 + i * strideA2 ];
		A[ offsetA + ( K + i ) * strideA1 + i * strideA2 ] = 1.0;

		// Compute Y(K+1:N, I)
		dgemv( 'no-transpose', N - K, N - K - i, 1.0, A, strideA1, strideA2, offsetA + K * strideA1 + ( i + 1 ) * strideA2, A, strideA1, offsetA + ( K + i ) * strideA1 + i * strideA2, 0.0, Y, strideY, offsetY + K * strideY + i * ldY );
		dgemv( 'transpose', N - K - i, i, 1.0, A, strideA1, strideA2, offsetA + ( K + i ) * strideA1, A, strideA1, offsetA + ( K + i ) * strideA1 + i * strideA2, 0.0, T, strideT, offsetT + i * ldT );
		dgemv( 'no-transpose', N - K, i, -1.0, Y, strideY, ldY, offsetY + K * strideY, T, strideT, offsetT + i * ldT, 1.0, Y, strideY, offsetY + K * strideY + i * ldY );
		dscal( N - K, tau[ offsetTAU + i * strideTAU ], Y, strideY, offsetY + K * strideY + i * ldY );

		// Compute T(1:I, I)
		dscal( i, -tau[ offsetTAU + i * strideTAU ], T, strideT, offsetT + i * ldT );
		dtrmv( 'upper', 'no-transpose', 'non-unit', i, T, strideT, ldT, offsetT, T, strideT, offsetT + i * ldT );
		T[ offsetT + i * strideT + i * ldT ] = tau[ offsetTAU + i * strideTAU ];
	}
	// Restore last saved subdiagonal element
	A[ offsetA + ( K + nb - 1 ) * strideA1 + ( nb - 1 ) * strideA2 ] = ei;

	// Compute Y(1:K, 1:NB)
	dlacpy( 'all', K, nb, A, strideA1, strideA2, offsetA + 1 * strideA2, Y, strideY, ldY, offsetY );
	dtrmm( 'right', 'lower', 'no-transpose', 'unit', K, nb, 1.0, A, strideA1, strideA2, offsetA + K * strideA1, Y, strideY, ldY, offsetY );
	if ( N > K + nb ) {
		dgemm( 'no-transpose', 'no-transpose', K, nb, N - K - nb, 1.0, A, strideA1, strideA2, offsetA + ( 1 + nb ) * strideA2, A, strideA1, strideA2, offsetA + ( K + nb ) * strideA1, 1.0, Y, strideY, ldY, offsetY );
	}
	dtrmm( 'right', 'upper', 'no-transpose', 'non-unit', K, nb, 1.0, T, strideT, ldT, offsetT, Y, strideY, ldY, offsetY );
}


// EXPORTS //

module.exports = dlahr2;
