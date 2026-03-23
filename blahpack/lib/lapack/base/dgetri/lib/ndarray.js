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
* Computes the inverse of a matrix using the LU factorization computed by dgetrf.
*
* @param {NonNegativeInteger} N - order of the matrix A
* @param {Float64Array} A - input/output matrix; on entry, the L and U factors from dgetrf; on exit, the inverse
* @param {integer} strideA1 - stride of the first dimension of A
* @param {integer} strideA2 - stride of the second dimension of A
* @param {NonNegativeInteger} offsetA - starting index for A
* @param {Int32Array} IPIV - pivot indices from dgetrf (0-based)
* @param {integer} strideIPIV - stride for IPIV
* @param {NonNegativeInteger} offsetIPIV - starting index for IPIV
* @param {Float64Array} WORK - workspace array of length at least max(1, lwork)
* @param {integer} strideWORK - stride for WORK
* @param {NonNegativeInteger} offsetWORK - starting index for WORK
* @param {integer} lwork - length of the WORK array; should be at least N for unblocked, N*NB for blocked
* @throws {RangeError} first argument must be a nonnegative integer
* @returns {integer} info - 0 if successful, k>0 if U(k,k) is exactly zero (singular)
*/
function dgetri( N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV, WORK, strideWORK, offsetWORK, lwork ) {
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( N === 0 ) {
		return 0;
	}
	return base( N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV, WORK, strideWORK, offsetWORK, lwork );
}


// EXPORTS //

module.exports = dgetri;
