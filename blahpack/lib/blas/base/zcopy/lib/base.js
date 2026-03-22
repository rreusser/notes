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

// MAIN //

/**
* Copy a complex double-precision vector.
*
* Complex elements are stored as interleaved real/imaginary pairs in a
* Float64Array. Element k of zx has real part at `offsetX + 2*k*strideX`
* and imaginary part at `offsetX + 2*k*strideX + 1`.
*
* @private
* @param {PositiveInteger} N - number of complex elements
* @param {Float64Array} zx - source array (interleaved complex)
* @param {integer} strideX - stride for `zx` (in complex elements)
* @param {NonNegativeInteger} offsetX - starting index for `zx`
* @param {Float64Array} zy - destination array (interleaved complex)
* @param {integer} strideY - stride for `zy` (in complex elements)
* @param {NonNegativeInteger} offsetY - starting index for `zy`
* @returns {Float64Array} `zy`
*/
function zcopy( N, zx, strideX, offsetX, zy, strideY, offsetY ) {
	var sx;
	var sy;
	var ix;
	var iy;
	var i;

	if ( N <= 0 ) {
		return zy;
	}

	// Each complex element spans 2 doubles, so multiply stride by 2
	sx = strideX * 2;
	sy = strideY * 2;
	ix = offsetX;
	iy = offsetY;

	for ( i = 0; i < N; i++ ) {
		zy[ iy ] = zx[ ix ];
		zy[ iy + 1 ] = zx[ ix + 1 ];
		ix += sx;
		iy += sy;
	}
	return zy;
}


// EXPORTS //

module.exports = zcopy;
