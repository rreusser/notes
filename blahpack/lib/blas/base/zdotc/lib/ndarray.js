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
* Compute the conjugate dot product of two complex vectors:.
*
* @param {NonNegativeInteger} N - number of complex elements
* @param {Complex128Array} x - first complex input vector
* @param {integer} strideX - stride for `x` (in complex elements)
* @param {NonNegativeInteger} offsetX - starting index for `x` (in complex elements)
* @param {Complex128Array} y - second complex input vector
* @param {integer} strideY - stride for `y` (in complex elements)
* @param {NonNegativeInteger} offsetY - starting index for `y` (in complex elements)
* @throws {RangeError} first argument must be a nonnegative integer
* @returns {Complex128} conjugate dot product
*/
function zdotc( N, x, strideX, offsetX, y, strideY, offsetY ) {
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( N === 0 ) {
		return 0;
	}
	return base( N, x, strideX, offsetX, y, strideY, offsetY );
}


// EXPORTS //

module.exports = zdotc;
