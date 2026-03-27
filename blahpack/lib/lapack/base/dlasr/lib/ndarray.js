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
* Applies a sequence of real plane rotations to a real general rectangular matrix.
*
* @param {string} side - `'left'` or `'right'`
* @param {string} pivot - `'variable'`, `'top'`, or `'bottom'`
* @param {string} direct - `'forward'` or `'backward'`
* @param {NonNegativeInteger} M - number of rows of A
* @param {NonNegativeInteger} N - number of columns of A
* @param {Float64Array} c - array of cosines
* @param {integer} strideC - stride for `c`
* @param {NonNegativeInteger} offsetC - starting index for `c`
* @param {Float64Array} s - array of sines
* @param {integer} strideS - stride for `s`
* @param {NonNegativeInteger} offsetS - starting index for `s`
* @param {Float64Array} A - input/output matrix
* @param {integer} strideA1 - stride of the first dimension of A
* @param {integer} strideA2 - stride of the second dimension of A
* @param {NonNegativeInteger} offsetA - starting index for A
* @throws {TypeError} first argument must be a valid operation side
* @throws {TypeError} second argument must be a valid pivot type
* @throws {TypeError} third argument must be a valid direction
* @throws {RangeError} fourth argument must be a nonnegative integer
* @throws {RangeError} fifth argument must be a nonnegative integer
* @returns {Float64Array} A
*/
function dlasr( side, pivot, direct, M, N, c, strideC, offsetC, s, strideS, offsetS, A, strideA1, strideA2, offsetA ) {
	if ( !isOperationSide( side ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid operation side. Value: `%s`.', side ) );
	}
	if ( pivot !== 'variable' && pivot !== 'top' && pivot !== 'bottom' ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid pivot type. Value: `%s`.', pivot ) );
	}
	if ( direct !== 'forward' && direct !== 'backward' ) {
		throw new TypeError( format( 'invalid argument. Third argument must be a valid direction. Value: `%s`.', direct ) );
	}
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Fifth argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( M === 0 || N === 0 ) {
		return 0;
	}
	return base( side, pivot, direct, M, N, c, strideC, offsetC, s, strideS, offsetS, A, strideA1, strideA2, offsetA );
}


// EXPORTS //

module.exports = dlasr;
