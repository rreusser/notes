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

var isOperationSide = require( '@stdlib/blas/base/assert/is-operation-side' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Applies a real elementary reflector H to a real M-by-N matrix C,.
*
* @param {string} side - 'L' for left, 'R' for right
* @param {NonNegativeInteger} M - number of rows of C
* @param {NonNegativeInteger} N - number of columns of C
* @param {Float64Array} v - reflector vector
* @param {integer} strideV - stride for v
* @param {NonNegativeInteger} offsetV - starting index for v
* @param {number} tau - scalar factor
* @param {Float64Array} C - matrix, modified in-place
* @param {integer} strideC1 - stride of first dimension of C
* @param {integer} strideC2 - stride of second dimension of C
* @param {NonNegativeInteger} offsetC - starting index for C
* @param {Float64Array} WORK - workspace array
* @param {integer} strideWORK - stride for WORK
* @param {NonNegativeInteger} offsetWORK - starting index for WORK
* @throws {TypeError} first argument must be a valid operation side
* @throws {RangeError} second argument must be a nonnegative integer
* @throws {RangeError} third argument must be a nonnegative integer
* @returns {*} result
*/
function dlarf( side, M, N, v, strideV, offsetV, tau, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK ) {
	if ( !isOperationSide( side ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid operation side. Value: `%s`.', side ) );
	}
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Third argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( M === 0 || N === 0 ) {
		return;
	}
	return base( side, M, N, v, strideV, offsetV, tau, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK );
}


// EXPORTS //

module.exports = dlarf;
