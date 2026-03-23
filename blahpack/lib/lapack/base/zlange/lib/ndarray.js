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
* Computes the value of a matrix norm for a complex matrix.
*
* @param {string} norm - 'M', '1', 'O', 'I', 'F', or 'E'
* @param {NonNegativeInteger} M - rows
* @param {NonNegativeInteger} N - columns
* @param {Complex128Array} A - complex matrix
* @param {integer} strideA1 - first dimension stride (in complex elements)
* @param {integer} strideA2 - second dimension stride (in complex elements)
* @param {NonNegativeInteger} offsetA - starting index for A (in complex elements)
* @param {Float64Array} WORK - workspace (length >= M for 'I' norm, real)
* @param {integer} strideWORK - stride for WORK
* @param {NonNegativeInteger} offsetWORK - starting index for WORK
* @throws {TypeError} first argument must be a valid norm type
* @throws {RangeError} second argument must be a nonnegative integer
* @throws {RangeError} third argument must be a nonnegative integer
* @returns {number} norm value
*/
function zlange( norm, M, N, A, strideA1, strideA2, offsetA, WORK, strideWORK, offsetWORK ) {
	if ( norm !== 'one-norm' && norm !== 'inf-norm' && norm !== 'max' && norm !== 'frobenius' ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid norm type. Value: `%s`.', norm ) );
	}
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Third argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( M === 0 || N === 0 ) {
		return 0.0;
	}
	return base( norm, M, N, A, strideA1, strideA2, offsetA, WORK, strideWORK, offsetWORK );
}


// EXPORTS //

module.exports = zlange;
