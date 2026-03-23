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

// MAIN //

/**
* Compute LU factorization of a complex banded matrix (blocked).
*
* @private
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {integer} kl - kl
* @param {integer} ku - ku
* @param {Float64Array} AB - input matrix
* @param {integer} strideAB1 - stride of the first dimension of `AB`
* @param {integer} strideAB2 - stride of the second dimension of `AB`
* @param {NonNegativeInteger} offsetAB - starting index for `AB`
* @param {Int32Array} IPIV - output array
* @param {integer} strideIPIV - stride length for `IPIV`
* @param {NonNegativeInteger} offsetIPIV - starting index for `IPIV`
* @returns {integer} status code (0 = success)
*/
function zgbtrf( M, N, kl, ku, AB, strideAB1, strideAB2, offsetAB, IPIV, strideIPIV, offsetIPIV ) {
	// TODO: implement
	throw new Error( 'not yet implemented' );
}


// EXPORTS //

module.exports = zgbtrf;
