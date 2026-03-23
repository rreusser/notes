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
* @param {TODO} ilo - TODO
* @param {TODO} ihi - TODO
* @param {TODO} A - TODO
* @param {TODO} strideA1 - TODO
* @param {TODO} strideA2 - TODO
* @param {TODO} offsetA - TODO
* @param {TODO} TAU - TODO
* @param {TODO} strideTAU - TODO
* @param {TODO} offsetTAU - TODO
* @param {TODO} WORK - TODO
* @param {TODO} strideWORK - TODO
* @param {TODO} offsetWORK - TODO
* @param {TODO} lwork - TODO
* @throws {RangeError} first argument must be a nonnegative integer
* @returns {*} result
*/
function zgehrd( N, ilo, ihi, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK, lwork ) {
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( N === 0 ) {
		return;
	}
	return base( N, ilo, ihi, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK, lwork );
}


// EXPORTS //

module.exports = zgehrd;
