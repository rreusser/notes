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
* Applies a plane rotation, where the cos (C) is real and the sin (S) is.
*
* @param {NonNegativeInteger} N - number of complex elements
* @param {Complex128Array} cx - first input/output array
* @param {integer} strideX - stride for `cx` (in complex elements)
* @param {NonNegativeInteger} offsetX - starting index for `cx` (in complex elements)
* @param {Complex128Array} cy - second input/output array
* @param {integer} strideY - stride for `cy` (in complex elements)
* @param {NonNegativeInteger} offsetY - starting index for `cy` (in complex elements)
* @param {number} c - cosine of rotation (real)
* @param {Float64Array} s - sine of rotation (complex, 2-element array [re, im])
* @throws {RangeError} first argument must be a nonnegative integer
* @returns {Complex128Array} cx
*/
function zrot( N, cx, strideX, offsetX, cy, strideY, offsetY, c, s ) {
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( N === 0 ) {
		return 0;
	}
	return base( N, cx, strideX, offsetX, cy, strideY, offsetY, c, s );
}


// EXPORTS //

module.exports = zrot;
