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

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Apply a complex Givens plane rotation
*
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} x - input array
* @param {integer} strideX - stride length for `x`
* @param {NonNegativeInteger} offsetX - starting index for `x`
* @param {integer} incx - incx
* @param {Float64Array} y - output array
* @param {integer} strideY - stride length for `y`
* @param {NonNegativeInteger} offsetY - starting index for `y`
* @param {integer} incy - incy
* @param {number} c - c
* @param {Complex128} s - s
*/
function zrot( N, x, strideX, offsetX, incx, y, strideY, offsetY, incy, c, s ) { // eslint-disable-line max-len, max-params
	return base( N, x, strideX, offsetX, incx, y, strideY, offsetY, incy, c, s ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zrot;
