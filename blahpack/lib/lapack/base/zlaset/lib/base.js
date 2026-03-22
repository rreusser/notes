/**
* @license Apache-2.0
*
* Copyright (c) 2025 Ricky Reusser.
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

// MAIN //

/**
* Initializes a complex matrix to BETA on the diagonal and ALPHA on the
* off-diagonals.
*
* Complex elements are stored as interleaved real/imaginary pairs in a
* Float64Array. Element (i, j) has real part at
* `offsetA + i*strideA1 + j*strideA2` and imaginary part at
* `offsetA + i*strideA1 + j*strideA2 + 1`.
*
* For complex matrices, strideA1 and strideA2 are in units of doubles.
* For column-major with LDA rows: strideA1 = 2, strideA2 = 2*LDA.
*
* @private
* @param {string} uplo - 'U' for upper, 'L' for lower, otherwise full
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} alpha - complex off-diagonal value [re, im]
* @param {Float64Array} beta - complex diagonal value [re, im]
* @param {Float64Array} A - input/output matrix (interleaved complex)
* @param {integer} strideA1 - stride of the first dimension of A (in doubles)
* @param {integer} strideA2 - stride of the second dimension of A (in doubles)
* @param {NonNegativeInteger} offsetA - starting index for A
* @returns {Float64Array} A
*/
function zlaset( uplo, M, N, alpha, beta, A, strideA1, strideA2, offsetA ) { // eslint-disable-line max-len, max-params
	var alphaRe;
	var alphaIm;
	var betaRe;
	var betaIm;
	var sa1;
	var sa2;
	var mn;
	var idx;
	var i;
	var j;

	alphaRe = alpha[ 0 ];
	alphaIm = alpha[ 1 ];
	betaRe = beta[ 0 ];
	betaIm = beta[ 1 ];
	sa1 = strideA1;
	sa2 = strideA2;
	mn = Math.min( M, N );

	if ( uplo === 'U' || uplo === 'u' ) {
		// Set the strictly upper triangular part to ALPHA.
		for ( j = 1; j < N; j++ ) {
			idx = offsetA + j * sa2;
			for ( i = 0; i < Math.min( j, M ); i++ ) {
				A[ idx ] = alphaRe;
				A[ idx + 1 ] = alphaIm;
				idx += sa1;
			}
		}
		// Set the diagonal to BETA.
		idx = offsetA;
		for ( i = 0; i < mn; i++ ) {
			A[ idx ] = betaRe;
			A[ idx + 1 ] = betaIm;
			idx += sa1 + sa2;
		}
	} else if ( uplo === 'L' || uplo === 'l' ) {
		// Set the strictly lower triangular part to ALPHA.
		for ( j = 0; j < Math.min( M, N ); j++ ) {
			idx = offsetA + ( j + 1 ) * sa1 + j * sa2;
			for ( i = j + 1; i < M; i++ ) {
				A[ idx ] = alphaRe;
				A[ idx + 1 ] = alphaIm;
				idx += sa1;
			}
		}
		// Set the diagonal to BETA.
		idx = offsetA;
		for ( i = 0; i < mn; i++ ) {
			A[ idx ] = betaRe;
			A[ idx + 1 ] = betaIm;
			idx += sa1 + sa2;
		}
	} else {
		// Set the full array to ALPHA, then overwrite diagonal with BETA.
		for ( j = 0; j < N; j++ ) {
			idx = offsetA + j * sa2;
			for ( i = 0; i < M; i++ ) {
				A[ idx ] = alphaRe;
				A[ idx + 1 ] = alphaIm;
				idx += sa1;
			}
		}
		idx = offsetA;
		for ( i = 0; i < mn; i++ ) {
			A[ idx ] = betaRe;
			A[ idx + 1 ] = betaIm;
			idx += sa1 + sa2;
		}
	}
	return A;
}


// EXPORTS //

module.exports = zlaset;
