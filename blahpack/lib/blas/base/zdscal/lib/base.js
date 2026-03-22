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
* Scale a complex double-precision vector by a double-precision constant.
*
* Complex elements are stored as interleaved real/imaginary pairs in a
* Float64Array. Element k of zx has real part at `offsetX + 2*k*strideX`
* and imaginary part at `offsetX + 2*k*strideX + 1`.
*
* @private
* @param {PositiveInteger} N - number of complex elements
* @param {number} da - real scalar multiplier
* @param {Float64Array} zx - input array (interleaved complex)
* @param {integer} strideX - stride for `zx` (in complex elements)
* @param {NonNegativeInteger} offsetX - starting index for `zx`
* @returns {Float64Array} `zx`
*/
function zdscal( N, da, zx, strideX, offsetX ) {
	var sx;
	var ix;
	var i;

	if ( N <= 0 ) {
		return zx;
	}

	// Each complex element spans 2 doubles, so multiply stride by 2
	sx = strideX * 2;
	ix = offsetX;

	for ( i = 0; i < N; i++ ) {
		zx[ ix ] *= da;
		zx[ ix + 1 ] *= da;
		ix += sx;
	}
	return zx;
}


// EXPORTS //

module.exports = zdscal;
