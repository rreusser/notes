/**
* @license Apache-2.0
*
* Copyright (c) 2025 Ricky Reusser.
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
* Applies a plane rotation, where the cos (C) is real and the sin (S) is
* complex, to a pair of complex vectors CX and CY:
*
*   CX(i) =  C * CX(i) + S * CY(i)
*   CY(i) = -conjg(S) * CX(i) + C * CY(i)
*
* Complex elements are stored as interleaved real/imaginary pairs in a
* Float64Array. Element k of cx has real part at `offsetX + 2*k*strideX`
* and imaginary part at `offsetX + 2*k*strideX + 1`.
*
* @private
* @param {NonNegativeInteger} N - number of complex elements
* @param {Float64Array} cx - first input/output array (interleaved complex)
* @param {integer} strideX - stride for `cx` (in complex elements)
* @param {NonNegativeInteger} offsetX - starting index for `cx`
* @param {Float64Array} cy - second input/output array (interleaved complex)
* @param {integer} strideY - stride for `cy` (in complex elements)
* @param {NonNegativeInteger} offsetY - starting index for `cy`
* @param {number} c - cosine of rotation (real)
* @param {Float64Array} s - sine of rotation (complex, 2-element array [re, im])
* @returns {Float64Array} cx
*/
function zrot( N, cx, strideX, offsetX, cy, strideY, offsetY, c, s ) {
	var stemp_r;
	var stemp_i;
	var cxr;
	var cxi;
	var cyr;
	var cyi;
	var sr;
	var si;
	var sx;
	var sy;
	var ix;
	var iy;
	var i;

	if ( N <= 0 ) {
		return cx;
	}

	sr = s[ 0 ];
	si = s[ 1 ];

	// Each complex element spans 2 doubles
	sx = strideX * 2;
	sy = strideY * 2;
	ix = offsetX;
	iy = offsetY;

	for ( i = 0; i < N; i++ ) {
		cxr = cx[ ix ];
		cxi = cx[ ix + 1 ];
		cyr = cy[ iy ];
		cyi = cy[ iy + 1 ];

		// stemp = c*cx(i) + s*cy(i)
		// s*cy(i) = (sr+si*i)*(cyr+cyi*i) = (sr*cyr - si*cyi) + (sr*cyi + si*cyr)*i
		stemp_r = c * cxr + ( sr * cyr - si * cyi );
		stemp_i = c * cxi + ( sr * cyi + si * cyr );

		// cy(i) = c*cy(i) - conjg(s)*cx(i)
		// conjg(s) = (sr, -si)
		// conjg(s)*cx(i) = (sr*cxr + si*cxi) + (-si*cxr + sr*cxi)*i
		cy[ iy ] = c * cyr - ( sr * cxr + si * cxi );
		cy[ iy + 1 ] = c * cyi - ( -si * cxr + sr * cxi );

		cx[ ix ] = stemp_r;
		cx[ ix + 1 ] = stemp_i;

		ix += sx;
		iy += sy;
	}
	return cx;
}


// EXPORTS //

module.exports = zrot;
