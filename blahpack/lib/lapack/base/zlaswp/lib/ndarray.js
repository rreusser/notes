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

var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Performs a series of row interchanges on a complex double-precision matrix `A`.
*
* @param {PositiveInteger} N - number of columns in `A`
* @param {Complex128Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A` (in complex elements)
* @param {integer} strideA2 - stride of the second dimension of `A` (in complex elements)
* @param {NonNegativeInteger} offsetA - index offset for `A` (in complex elements)
* @param {NonNegativeInteger} k1 - index of first row to interchange (0-based)
* @param {NonNegativeInteger} k2 - index of last row to interchange (0-based)
* @param {Int32Array} IPIV - vector of pivot indices (0-based)
* @param {integer} strideIPIV - `IPIV` stride length
* @param {NonNegativeInteger} offsetIPIV - index offset for `IPIV`
* @param {integer} incx - direction in which to apply pivots (-1 to apply in reverse order; otherwise, apply in provided order)
* @throws {RangeError} first argument must be a nonnegative integer
* @returns {Complex128Array} permuted matrix `A`
*/
function zlaswp( N, A, strideA1, strideA2, offsetA, k1, k2, IPIV, strideIPIV, offsetIPIV, incx ) {
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( N === 0 ) {
		return A;
	}
	return base( N, A, strideA1, strideA2, offsetA, k1, k2, IPIV, strideIPIV, offsetIPIV, incx );
}


// EXPORTS //

module.exports = zlaswp;
