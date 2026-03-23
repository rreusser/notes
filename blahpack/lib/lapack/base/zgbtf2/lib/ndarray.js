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
* Computes an LU factorization of a complex M-by-N band matrix A using partial.
*
* @param {NonNegativeInteger} M - number of rows of matrix A
* @param {NonNegativeInteger} N - number of columns of matrix A
* @param {NonNegativeInteger} kl - number of subdiagonals
* @param {NonNegativeInteger} ku - number of superdiagonals
* @param {Complex128Array} AB - band matrix in band storage
* @param {integer} strideAB1 - stride of the first dimension of AB (in complex elements)
* @param {integer} strideAB2 - stride of the second dimension of AB (in complex elements)
* @param {NonNegativeInteger} offsetAB - starting index for AB (in complex elements)
* @param {Int32Array} IPIV - pivot index output array, length min(M,N)
* @param {integer} strideIPIV - stride for IPIV
* @param {NonNegativeInteger} offsetIPIV - starting index for IPIV
* @throws {RangeError} first argument must be a nonnegative integer
* @throws {RangeError} second argument must be a nonnegative integer
* @returns {integer} info - 0 if successful, k if U(k-1,k-1) is exactly zero (1-based)
*/
function zgbtf2( M, N, kl, ku, AB, strideAB1, strideAB2, offsetAB, IPIV, strideIPIV, offsetIPIV ) {
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( M === 0 || N === 0 ) {
		return 0;
	}
	return base( M, N, kl, ku, AB, strideAB1, strideAB2, offsetAB, IPIV, strideIPIV, offsetIPIV );
}


// EXPORTS //

module.exports = zgbtf2;
