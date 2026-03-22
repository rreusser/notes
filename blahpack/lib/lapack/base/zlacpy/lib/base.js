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
* Copies all or part of a complex matrix `A` to another complex matrix `B`.
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
* @param {string} uplo - specifies whether to copy the upper triangle, lower triangle, or all of `A`
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix (interleaved complex)
* @param {integer} strideA1 - stride of the first dimension of `A` (in doubles)
* @param {integer} strideA2 - stride of the second dimension of `A` (in doubles)
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} B - output matrix (interleaved complex)
* @param {integer} strideB1 - stride of the first dimension of `B` (in doubles)
* @param {integer} strideB2 - stride of the second dimension of `B` (in doubles)
* @param {NonNegativeInteger} offsetB - starting index for `B`
* @returns {Float64Array} `B`
*/
function zlacpy( uplo, M, N, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB ) { // eslint-disable-line max-len, max-params
	var ia;
	var ib;
	var i;
	var j;

	if ( uplo === 'U' || uplo === 'u' ) {
		for ( j = 0; j < N; j++ ) {
			ia = offsetA + ( j * strideA2 );
			ib = offsetB + ( j * strideB2 );
			for ( i = 0; i <= j && i < M; i++ ) {
				B[ ib + ( i * strideB1 ) ] = A[ ia + ( i * strideA1 ) ];
				B[ ib + ( i * strideB1 ) + 1 ] = A[ ia + ( i * strideA1 ) + 1 ];
			}
		}
	} else if ( uplo === 'L' || uplo === 'l' ) {
		for ( j = 0; j < N; j++ ) {
			ia = offsetA + ( j * strideA2 );
			ib = offsetB + ( j * strideB2 );
			for ( i = j; i < M; i++ ) {
				B[ ib + ( i * strideB1 ) ] = A[ ia + ( i * strideA1 ) ];
				B[ ib + ( i * strideB1 ) + 1 ] = A[ ia + ( i * strideA1 ) + 1 ];
			}
		}
	} else {
		for ( j = 0; j < N; j++ ) {
			ia = offsetA + ( j * strideA2 );
			ib = offsetB + ( j * strideB2 );
			for ( i = 0; i < M; i++ ) {
				B[ ib + ( i * strideB1 ) ] = A[ ia + ( i * strideA1 ) ];
				B[ ib + ( i * strideB1 ) + 1 ] = A[ ia + ( i * strideA1 ) + 1 ];
			}
		}
	}
	return B;
}


// EXPORTS //

module.exports = zlacpy;
