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

// VARIABLES //

var BLOCK_SIZE = 32;


// MAIN //

/**
* Performs a series of row interchanges on a matrix `A` using pivot indices stored in `IPIV`.
*
* When incx > 0, rows k1 through k2 are interchanged in forward order, reading
* IPIV from offsetIPIV.
*
* When incx < 0, rows k1 down to k2 are interchanged in reverse order (k1 > k2),
* reading IPIV from offsetIPIV + (k1-k2)*strideIPIV backwards.
*
* @private
* @param {PositiveInteger} N - number of columns in `A`
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - index offset for `A`
* @param {NonNegativeInteger} k1 - index of first row to interchange
* @param {NonNegativeInteger} k2 - index of last row to interchange
* @param {Int32Array} IPIV - vector of pivot indices
* @param {integer} strideIPIV - `IPIV` stride length
* @param {NonNegativeInteger} offsetIPIV - index offset for `IPIV`
* @param {integer} incx - direction in which to apply pivots (-1 to apply in reverse order; otherwise, apply in provided order)
* @returns {Float64Array} permuted matrix `A`
*/
function dlaswp( N, A, strideA1, strideA2, offsetA, k1, k2, IPIV, strideIPIV, offsetIPIV, incx ) {
	var istart;
	var nrows;
	var ixinc;
	var iinc;
	var ix0;
	var n32;
	var tmp;
	var row;
	var ia1;
	var ia2;
	var ix;
	var kk;
	var i;
	var j;
	var n;
	var o;

	if ( incx > 0 ) {
		// Forward: iterate from k1 to k2
		nrows = k2 - k1 + 1;
		ix0 = offsetIPIV;
		ixinc = strideIPIV;
		istart = k1;
		iinc = 1;
	} else if ( incx < 0 ) {
		// Reverse: k1 > k2, iterate from k1 down to k2
		nrows = k1 - k2 + 1;

		// IPIV is read from the last element backwards
		ix0 = offsetIPIV + (( nrows - 1 ) * strideIPIV);
		ixinc = -strideIPIV;
		istart = k1;
		iinc = -1;
	} else {
		return A;
	}

	// Use loop tiling for cache-efficient column-major access.
	// Outer loop: blocks of columns. Inner loop: row swaps.
	n32 = ( ( N / BLOCK_SIZE ) | 0 ) * BLOCK_SIZE;
	if ( n32 !== 0 ) {
		for ( j = 0; j < n32; j += BLOCK_SIZE ) {
			ix = ix0;
			kk = istart;
			for ( i = 0; i < nrows; i++ ) {
				row = IPIV[ ix ];
				if ( row !== kk ) {
					ia1 = offsetA + ( kk * strideA1 );
					ia2 = offsetA + ( row * strideA1 );
					for ( n = j; n < j + BLOCK_SIZE; n++ ) {
						o = n * strideA2;
						tmp = A[ ia1 + o ];
						A[ ia1 + o ] = A[ ia2 + o ];
						A[ ia2 + o ] = tmp;
					}
				}
				ix += ixinc;
				kk += iinc;
			}
		}
	}
	if ( n32 !== N ) {
		ix = ix0;
		kk = istart;
		for ( i = 0; i < nrows; i++ ) {
			row = IPIV[ ix ];
			if ( row !== kk ) {
				ia1 = offsetA + ( kk * strideA1 );
				ia2 = offsetA + ( row * strideA1 );
				for ( n = n32; n < N; n++ ) {
					o = n * strideA2;
					tmp = A[ ia1 + o ];
					A[ ia1 + o ] = A[ ia2 + o ];
					A[ ia2 + o ] = tmp;
				}
			}
			ix += ixinc;
			kk += iinc;
		}
	}
	return A;
}


// EXPORTS //

module.exports = dlaswp;
