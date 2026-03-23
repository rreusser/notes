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
* Computes the value of the one norm, or the Frobenius norm, or the infinity.
*
* @param {string} norm - norm type: 'M' (max), '1'/'O' (one-norm), 'I' (infinity), 'F'/'E' (Frobenius)
* @param {NonNegativeInteger} N - order of the matrix A
* @param {Float64Array} d - diagonal elements, length N
* @param {integer} strideD - stride for d
* @param {NonNegativeInteger} offsetD - starting index for d
* @param {Float64Array} e - off-diagonal elements, length N-1
* @param {integer} strideE - stride for e
* @param {NonNegativeInteger} offsetE - starting index for e
* @throws {TypeError} first argument must be a valid norm type
* @throws {RangeError} second argument must be a nonnegative integer
* @returns {number} the computed norm value
*/
function dlanst( norm, N, d, strideD, offsetD, e, strideE, offsetE ) {
	if ( norm !== 'one-norm' && norm !== 'inf-norm' && norm !== 'max' && norm !== 'frobenius' ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid norm type. Value: `%s`.', norm ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( N === 0 ) {
		return 0.0;
	}
	return base( norm, N, d, strideD, offsetD, e, strideE, offsetE );
}


// EXPORTS //

module.exports = dlanst;
