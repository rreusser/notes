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
* Copyright (c) 2025 The Stdlib Authors.
*
* @param {TODO} N - TODO
* @param {TODO} K - TODO
* @param {TODO} nb - TODO
* @param {TODO} A - TODO
* @param {TODO} strideA1 - TODO
* @param {TODO} strideA2 - TODO
* @param {TODO} offsetA - TODO
* @param {TODO} tau - TODO
* @param {TODO} strideTAU - TODO
* @param {TODO} offsetTAU - TODO
* @param {TODO} T - TODO
* @param {TODO} strideT1 - TODO
* @param {TODO} strideT2 - TODO
* @param {TODO} offsetT - TODO
* @param {TODO} Y - TODO
* @param {TODO} strideY1 - TODO
* @param {TODO} strideY2 - TODO
* @param {TODO} offsetY - TODO
* @throws {RangeError} first argument must be a nonnegative integer
* @throws {RangeError} second argument must be a nonnegative integer
* @throws {RangeError} third argument must be a nonnegative integer
* @returns {*} result
*/
function zlahr2( N, K, nb, A, strideA1, strideA2, offsetA, tau, strideTAU, offsetTAU, T, strideT1, strideT2, offsetT, Y, strideY1, strideY2, offsetY ) {
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( K < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', K ) );
	}
	if ( nb < 0 ) {
		throw new RangeError( format( 'invalid argument. Third argument must be a nonnegative integer. Value: `%d`.', nb ) );
	}
	if ( N === 0 ) {
		return;
	}
	return base( N, K, nb, A, strideA1, strideA2, offsetA, tau, strideTAU, offsetTAU, T, strideT1, strideT2, offsetT, Y, strideY1, strideY2, offsetY );
}


// EXPORTS //

module.exports = zlahr2;
