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
* Solves a system of linear equations A.
*
* @param {NonNegativeInteger} N - order of the matrix A
* @param {NonNegativeInteger} kl - number of subdiagonals
* @param {NonNegativeInteger} ku - number of superdiagonals
* @param {NonNegativeInteger} nrhs - number of right-hand sides (columns of B)
* @param {Float64Array} AB - band matrix in LAPACK band storage (2*KL+KU+1 by N)
* @param {integer} strideAB1 - stride of the first dimension of AB
* @param {integer} strideAB2 - stride of the second dimension of AB
* @param {NonNegativeInteger} offsetAB - starting index for AB
* @param {Int32Array} IPIV - pivot index array of length N (output, 0-based)
* @param {integer} strideIPIV - stride for IPIV
* @param {NonNegativeInteger} offsetIPIV - starting index for IPIV
* @param {Float64Array} B - right-hand side matrix (N by NRHS)
* @param {integer} strideB1 - stride of the first dimension of B
* @param {integer} strideB2 - stride of the second dimension of B
* @param {NonNegativeInteger} offsetB - starting index for B
* @throws {RangeError} first argument must be a nonnegative integer
* @throws {RangeError} fourth argument must be a nonnegative integer
* @returns {integer} info - 0 if successful, >0 if U(i,i) is zero (1-based index)
*/
function dgbsv( N, kl, ku, nrhs, AB, strideAB1, strideAB2, offsetAB, IPIV, strideIPIV, offsetIPIV, B, strideB1, strideB2, offsetB ) {
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( nrhs < 0 ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be a nonnegative integer. Value: `%d`.', nrhs ) );
	}
	if ( N === 0 ) {
		return 0;
	}
	return base( N, kl, ku, nrhs, AB, strideAB1, strideAB2, offsetAB, IPIV, strideIPIV, offsetIPIV, B, strideB1, strideB2, offsetB );
}


// EXPORTS //

module.exports = dgbsv;
