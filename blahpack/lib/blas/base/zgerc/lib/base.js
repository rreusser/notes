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
* Perform the rank 1 operation A := alpha*x*y**H + A,
* where alpha is a complex scalar, x is an M element complex vector,
* y is an N element complex vector, and A is an M by N complex matrix.
*
* Complex elements are stored as interleaved real/imaginary pairs.
* Element k of x has real part at `offsetX + 2*k*strideX` and
* imaginary part at `offsetX + 2*k*strideX + 1`.
*
* @private
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} alpha - complex scalar [re, im]
* @param {Float64Array} x - first input vector (interleaved complex)
* @param {integer} strideX - stride for `x` (in complex elements)
* @param {NonNegativeInteger} offsetX - starting index for `x`
* @param {Float64Array} y - second input vector (interleaved complex)
* @param {integer} strideY - stride for `y` (in complex elements)
* @param {NonNegativeInteger} offsetY - starting index for `y`
* @param {Float64Array} A - matrix (interleaved complex)
* @param {integer} strideA1 - stride of the first dimension of `A` (in complex elements)
* @param {integer} strideA2 - stride of the second dimension of `A` (in complex elements)
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @returns {Float64Array} `A`
*/
function zgerc( M, N, alpha, x, strideX, offsetX, y, strideY, offsetY, A, strideA1, strideA2, offsetA ) { // eslint-disable-line max-len, max-params
	var alphaRe;
	var alphaIm;
	var temp;
	var sa1;
	var sa2;
	var sx;
	var sy;
	var ix;
	var jy;
	var ia;
	var i;
	var j;
	var yr;
	var yi;
	var tr;
	var ti;

	if ( M <= 0 || N <= 0 ) {
		return A;
	}

	alphaRe = alpha[ 0 ];
	alphaIm = alpha[ 1 ];

	// Quick return if alpha is zero
	if ( alphaRe === 0.0 && alphaIm === 0.0 ) {
		return A;
	}

	// Convert strides from complex-element units to double units
	sx = strideX * 2;
	sy = strideY * 2;
	sa1 = strideA1 * 2;
	sa2 = strideA2 * 2;

	temp = new Float64Array( 2 );
	jy = offsetY;

	for ( j = 0; j < N; j++ ) {
		yr = y[ jy ];
		yi = y[ jy + 1 ];

		// Check if y(j) is zero
		if ( yr !== 0.0 || yi !== 0.0 ) {
			// temp = alpha * conj(y(j))
			// conj(y) = yr - yi*i
			// alpha * conj(y) = (alphaRe + alphaIm*i)(yr - yi*i)
			//                 = (alphaRe*yr + alphaIm*yi) + (alphaIm*yr - alphaRe*yi)*i
			tr = alphaRe * yr + alphaIm * yi;
			ti = alphaIm * yr - alphaRe * yi;

			ix = offsetX;
			ia = offsetA + j * sa2;
			for ( i = 0; i < M; i++ ) {
				// A(i,j) = A(i,j) + x(i) * temp
				// (xr + xi*i)(tr + ti*i) = (xr*tr - xi*ti) + (xr*ti + xi*tr)*i
				A[ ia ] += x[ ix ] * tr - x[ ix + 1 ] * ti;
				A[ ia + 1 ] += x[ ix ] * ti + x[ ix + 1 ] * tr;
				ix += sx;
				ia += sa1;
			}
		}
		jy += sy;
	}
	return A;
}


// EXPORTS //

module.exports = zgerc;
