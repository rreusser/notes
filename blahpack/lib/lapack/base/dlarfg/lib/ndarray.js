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
* Generates a real elementary reflector H of order N, such that.
*
* @param {NonNegativeInteger} N - order of the reflector
* @param {Float64Array} alpha - scalar, overwritten with beta on exit
* @param {NonNegativeInteger} offsetAlpha - index into alpha array
* @param {Float64Array} x - vector, overwritten with v on exit
* @param {integer} strideX - stride for x
* @param {NonNegativeInteger} offsetX - starting index for x
* @param {Float64Array} tau - output scalar
* @param {NonNegativeInteger} offsetTau - index into tau array
* @throws {RangeError} first argument must be a nonnegative integer
* @returns {*} result
*/
function dlarfg( N, alpha, offsetAlpha, x, strideX, offsetX, tau, offsetTau ) {
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( N === 0 ) {
		return;
	}
	return base( N, alpha, offsetAlpha, x, strideX, offsetX, tau, offsetTau );
}


// EXPORTS //

module.exports = dlarfg;
