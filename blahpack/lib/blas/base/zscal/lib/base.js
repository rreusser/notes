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

var cmplx = require( '../../../../cmplx.js' );

// MAIN //

/**
* Scale a complex double-precision vector by a complex constant.
*
* Complex elements are stored as interleaved real/imaginary pairs in a
* Float64Array. Element k of zx has real part at `offsetX + 2*k*strideX`
* and imaginary part at `offsetX + 2*k*strideX + 1`.
*
* @private
* @param {PositiveInteger} N - number of complex elements
* @param {Float64Array} za - complex scalar (2-element array: [real, imag])
* @param {Float64Array} zx - input array (interleaved complex)
* @param {integer} strideX - stride for `zx` (in complex elements)
* @param {NonNegativeInteger} offsetX - starting index for `zx`
* @returns {Float64Array} `zx`
*/
function zscal( N, za, zx, strideX, offsetX ) {
	var tmp;
	var sx;
	var ix;
	var i;

	if ( N <= 0 ) {
		return zx;
	}

	// Early return if za === (1, 0)
	if ( za[ 0 ] === 1.0 && za[ 1 ] === 0.0 ) {
		return zx;
	}

	tmp = new Float64Array( 2 );

	// Each complex element spans 2 doubles, so multiply stride by 2
	sx = strideX * 2;
	ix = offsetX;

	for ( i = 0; i < N; i++ ) {
		cmplx.mul( tmp, za, zx.subarray( ix, ix + 2 ) );
		zx[ ix ] = tmp[ 0 ];
		zx[ ix + 1 ] = tmp[ 1 ];
		ix += sx;
	}
	return zx;
}


// EXPORTS //

module.exports = zscal;
