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
* Computes all the singular values of a real upper bidiagonal matrix of.
*
* @param {NonNegativeInteger} N - number of rows and columns
* @param {Float64Array} d - diagonal elements of the bidiagonal matrix, length N
* @param {integer} strideD - stride length for `d`
* @param {NonNegativeInteger} offsetD - starting index for `d`
* @param {Float64Array} e - off-diagonal elements of the bidiagonal matrix, length N-1
* @param {integer} strideE - stride length for `e`
* @param {NonNegativeInteger} offsetE - starting index for `e`
* @param {Float64Array} WORK - workspace array of length at least 4*N
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @throws {RangeError} first argument must be a nonnegative integer
* @returns {integer} info - status code (0 = success)
*/
function dlasq1( N, d, strideD, offsetD, e, strideE, offsetE, WORK, strideWORK, offsetWORK ) {
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( N === 0 ) {
		return 0;
	}
	return base( N, d, strideD, offsetD, e, strideE, offsetE, WORK, strideWORK, offsetWORK );
}


// EXPORTS //

module.exports = dlasq1;
