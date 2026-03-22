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
* Perform one of the complex matrix-vector operations:
*   y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,   or
*   y := alpha*A**H*x + beta*y
*
* Complex elements are stored as interleaved real/imaginary pairs in
* Float64Arrays. Element k of a complex vector has real part at offset + 2*k*stride
* and imaginary part at offset + 2*k*stride + 1.
*
* Complex element (i,j) of matrix A is at:
*   real: offsetA + 2*(i*strideA1 + j*strideA2)
*   imag: offsetA + 2*(i*strideA1 + j*strideA2) + 1
*
* @private
* @param {string} trans - specifies the operation ('N', 'T', or 'C')
* @param {NonNegativeInteger} M - number of rows of A
* @param {NonNegativeInteger} N - number of columns of A
* @param {Float64Array} alpha - complex scalar (2-element array)
* @param {Float64Array} A - input matrix (interleaved complex)
* @param {integer} strideA1 - stride of the first dimension of A
* @param {integer} strideA2 - stride of the second dimension of A
* @param {NonNegativeInteger} offsetA - starting index for A
* @param {Float64Array} x - input vector (interleaved complex)
* @param {integer} strideX - stride for x (in complex elements)
* @param {NonNegativeInteger} offsetX - starting index for x
* @param {Float64Array} beta - complex scalar (2-element array)
* @param {Float64Array} y - input/output vector (interleaved complex)
* @param {integer} strideY - stride for y (in complex elements)
* @param {NonNegativeInteger} offsetY - starting index for y
* @returns {Float64Array} `y`
*/
function zgemv( trans, M, N, alpha, A, strideA1, strideA2, offsetA, x, strideX, offsetX, beta, y, strideY, offsetY ) { // eslint-disable-line max-len, max-params
	var noTrans;
	var noConj;
	var alphaR;
	var alphaI;
	var betaR;
	var betaI;
	var tempR;
	var tempI;
	var lenx;
	var leny;
	var aijR;
	var aijI;
	var sa1;
	var sa2;
	var sx;
	var sy;
	var ix;
	var iy;
	var jx;
	var jy;
	var ai;
	var i;
	var j;

	if ( M === 0 || N === 0 ) {
		return y;
	}

	alphaR = alpha[ 0 ];
	alphaI = alpha[ 1 ];
	betaR = beta[ 0 ];
	betaI = beta[ 1 ];

	// Quick return if alpha=0 and beta=1
	if ( alphaR === 0.0 && alphaI === 0.0 && betaR === 1.0 && betaI === 0.0 ) {
		return y;
	}

	noTrans = ( trans === 'N' || trans === 'n' );
	noConj = ( trans === 'T' || trans === 't' );

	if ( noTrans ) {
		lenx = N;
		leny = M;
	} else {
		lenx = M;
		leny = N;
	}

	// Matrix strides are in complex elements, so multiply by 2
	sa1 = strideA1 * 2;
	sa2 = strideA2 * 2;

	// Vector strides in complex elements, multiply by 2
	sx = strideX * 2;
	sy = strideY * 2;

	// First form y := beta*y
	if ( betaR !== 1.0 || betaI !== 0.0 ) {
		iy = offsetY;
		if ( betaR === 0.0 && betaI === 0.0 ) {
			for ( i = 0; i < leny; i++ ) {
				y[ iy ] = 0.0;
				y[ iy + 1 ] = 0.0;
				iy += sy;
			}
		} else {
			for ( i = 0; i < leny; i++ ) {
				tempR = betaR * y[ iy ] - betaI * y[ iy + 1 ];
				tempI = betaR * y[ iy + 1 ] + betaI * y[ iy ];
				y[ iy ] = tempR;
				y[ iy + 1 ] = tempI;
				iy += sy;
			}
		}
	}

	if ( alphaR === 0.0 && alphaI === 0.0 ) {
		return y;
	}

	if ( noTrans ) {
		// Form y := alpha*A*x + y
		jx = offsetX;
		for ( j = 0; j < N; j++ ) {
			// temp = alpha * x[jx]
			tempR = alphaR * x[ jx ] - alphaI * x[ jx + 1 ];
			tempI = alphaR * x[ jx + 1 ] + alphaI * x[ jx ];
			iy = offsetY;
			ai = offsetA + j * sa2;
			for ( i = 0; i < M; i++ ) {
				aijR = A[ ai ];
				aijI = A[ ai + 1 ];
				// y[iy] += temp * A[i,j]
				y[ iy ] += tempR * aijR - tempI * aijI;
				y[ iy + 1 ] += tempR * aijI + tempI * aijR;
				iy += sy;
				ai += sa1;
			}
			jx += sx;
		}
	} else {
		// Form y := alpha*A**T*x + y  or  y := alpha*A**H*x + y
		jy = offsetY;
		for ( j = 0; j < N; j++ ) {
			tempR = 0.0;
			tempI = 0.0;
			ix = offsetX;
			ai = offsetA + j * sa2;
			if ( noConj ) {
				// Transpose (no conjugate)
				for ( i = 0; i < M; i++ ) {
					aijR = A[ ai ];
					aijI = A[ ai + 1 ];
					// temp += A[i,j] * x[ix]
					tempR += aijR * x[ ix ] - aijI * x[ ix + 1 ];
					tempI += aijR * x[ ix + 1 ] + aijI * x[ ix ];
					ix += sx;
					ai += sa1;
				}
			} else {
				// Conjugate transpose
				for ( i = 0; i < M; i++ ) {
					aijR = A[ ai ];
					aijI = -A[ ai + 1 ]; // conjugate
					// temp += conj(A[i,j]) * x[ix]
					tempR += aijR * x[ ix ] - aijI * x[ ix + 1 ];
					tempI += aijR * x[ ix + 1 ] + aijI * x[ ix ];
					ix += sx;
					ai += sa1;
				}
			}
			// y[jy] += alpha * temp
			y[ jy ] += alphaR * tempR - alphaI * tempI;
			y[ jy + 1 ] += alphaR * tempI + alphaI * tempR;
			jy += sy;
		}
	}
	return y;
}


// EXPORTS //

module.exports = zgemv;
