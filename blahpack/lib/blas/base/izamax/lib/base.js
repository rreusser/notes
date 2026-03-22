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
* Finds the index of the element having the maximum sum of absolute values of
* real and imaginary parts in a double-precision complex vector.
*
* Complex elements are stored as interleaved real/imaginary pairs in a
* Float64Array. The stride is in units of complex elements (each occupying
* 2 consecutive Float64 values).
*
* @param {NonNegativeInteger} N - number of complex elements
* @param {Float64Array} zx - input complex vector (interleaved real/imag)
* @param {integer} strideX - stride in complex elements
* @param {NonNegativeInteger} offsetX - starting index in the Float64Array
* @returns {integer} 0-based index of the max element, or -1 if N < 1
*/
function izamax( N, zx, strideX, offsetX ) {
	var dmax;
	var imax;
	var step;
	var val;
	var ix;
	var i;

	if ( N < 1 || strideX <= 0 ) {
		return -1;
	}
	if ( N === 1 ) {
		return 0;
	}

	// Step size in Float64 indices for each complex element
	step = 2 * strideX;
	ix = offsetX;
	dmax = Math.abs( zx[ ix ] ) + Math.abs( zx[ ix + 1 ] );
	imax = 0;
	ix += step;

	for ( i = 1; i < N; i++ ) {
		val = Math.abs( zx[ ix ] ) + Math.abs( zx[ ix + 1 ] );
		if ( val > dmax ) {
			imax = i;
			dmax = val;
		}
		ix += step;
	}
	return imax;
}


// EXPORTS //

module.exports = izamax;
