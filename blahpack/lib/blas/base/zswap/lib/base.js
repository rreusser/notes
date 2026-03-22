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
* Interchange two complex double-precision vectors.
*
* Complex elements are stored as interleaved real/imaginary pairs in a
* Float64Array. Element k of zx has real part at `offsetX + 2*k*strideX`
* and imaginary part at `offsetX + 2*k*strideX + 1`.
*
* @private
* @param {PositiveInteger} N - number of complex elements
* @param {Float64Array} zx - first input array (interleaved complex)
* @param {integer} strideX - stride for `zx` (in complex elements)
* @param {NonNegativeInteger} offsetX - starting index for `zx`
* @param {Float64Array} zy - second input array (interleaved complex)
* @param {integer} strideY - stride for `zy` (in complex elements)
* @param {NonNegativeInteger} offsetY - starting index for `zy`
* @returns {Float64Array} `zx`
*/
function zswap( N, zx, strideX, offsetX, zy, strideY, offsetY ) {
	var tmp0;
	var tmp1;
	var sx;
	var sy;
	var ix;
	var iy;
	var i;

	if ( N <= 0 ) {
		return zx;
	}

	// Each complex element spans 2 doubles, so multiply stride by 2
	sx = strideX * 2;
	sy = strideY * 2;
	ix = offsetX;
	iy = offsetY;

	for ( i = 0; i < N; i++ ) {
		tmp0 = zx[ ix ];
		tmp1 = zx[ ix + 1 ];
		zx[ ix ] = zy[ iy ];
		zx[ ix + 1 ] = zy[ iy + 1 ];
		zy[ iy ] = tmp0;
		zy[ iy + 1 ] = tmp1;
		ix += sx;
		iy += sy;
	}
	return zx;
}


// EXPORTS //

module.exports = zswap;
