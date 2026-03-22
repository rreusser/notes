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
* Conjugate a complex vector in-place.
*
* Complex elements are stored as interleaved real/imaginary pairs.
* Element k has real part at `offset + 2*k*stride` and imaginary part
* at `offset + 2*k*stride + 1`.
*
* @private
* @param {NonNegativeInteger} N - number of complex elements
* @param {Float64Array} x - input array (interleaved complex)
* @param {integer} stride - stride for `x` (in complex elements)
* @param {NonNegativeInteger} offset - starting index for `x`
* @returns {Float64Array} `x`
*/
function zlacgv( N, x, stride, offset ) {
	var sx;
	var ix;
	var i;

	if ( N <= 0 ) {
		return x;
	}

	sx = stride * 2;
	ix = offset;
	for ( i = 0; i < N; i++ ) {
		x[ ix + 1 ] = -x[ ix + 1 ];
		ix += sx;
	}
	return x;
}


// EXPORTS //

module.exports = zlacgv;
