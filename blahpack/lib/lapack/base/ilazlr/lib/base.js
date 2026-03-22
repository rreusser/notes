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
* Scans a complex matrix for its last non-zero row.
*
* Complex elements are stored as interleaved real/imaginary pairs in a
* Float64Array. Element (i, j) has real part at
* `offsetA + i*strideA1 + j*strideA2` and imaginary part at
* `offsetA + i*strideA1 + j*strideA2 + 1`.
*
* For complex matrices, strideA1 and strideA2 are in units of doubles
* (not complex elements). For column-major with LDA rows:
* strideA1 = 2, strideA2 = 2*LDA.
*
* @private
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix (interleaved complex)
* @param {integer} strideA1 - stride of the first dimension of A (in doubles)
* @param {integer} strideA2 - stride of the second dimension of A (in doubles)
* @param {NonNegativeInteger} offsetA - starting index for A
* @returns {integer} 0-based index of last non-zero row, or -1 if none
*/
function ilazlr( M, N, A, strideA1, strideA2, offsetA ) {
	var result;
	var sa1;
	var sa2;
	var re;
	var im;
	var i;
	var j;

	if ( M === 0 ) {
		return -1;
	}

	sa1 = strideA1;
	sa2 = strideA2;

	// Quick test for the common case where one corner is non-zero.
	re = A[ offsetA + ( M - 1 ) * sa1 + 0 * sa2 ];
	im = A[ offsetA + ( M - 1 ) * sa1 + 0 * sa2 + 1 ];
	if ( re !== 0.0 || im !== 0.0 ) {
		return M - 1;
	}
	re = A[ offsetA + ( M - 1 ) * sa1 + ( N - 1 ) * sa2 ];
	im = A[ offsetA + ( M - 1 ) * sa1 + ( N - 1 ) * sa2 + 1 ];
	if ( re !== 0.0 || im !== 0.0 ) {
		return M - 1;
	}

	// Scan up each column tracking the last zero row seen.
	result = -1;
	for ( j = 0; j < N; j++ ) {
		i = M - 1;
		while ( i >= 0 ) {
			re = A[ offsetA + i * sa1 + j * sa2 ];
			im = A[ offsetA + i * sa1 + j * sa2 + 1 ];
			if ( re !== 0.0 || im !== 0.0 ) {
				break;
			}
			i--;
		}
		if ( i > result ) {
			result = i;
		}
	}
	return result;
}


// EXPORTS //

module.exports = ilazlr;
