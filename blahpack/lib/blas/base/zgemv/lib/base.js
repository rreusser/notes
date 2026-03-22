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

var float64view = require( '../../../../float64view.js' );
var complexParts = float64view.complexParts;

// MAIN //

/**
* Perform one of the complex matrix-vector operations:
*   y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,   or
*   y := alpha*A**H*x + beta*y
*
* @private
* @param {string} trans - specifies the operation ('N', 'T', or 'C')
* @param {NonNegativeInteger} M - number of rows of A
* @param {NonNegativeInteger} N - number of columns of A
* @param {Complex128} alpha - complex scalar
* @param {Complex128Array} A - complex input matrix
* @param {integer} strideA1 - stride of the first dimension of A
* @param {integer} strideA2 - stride of the second dimension of A
* @param {NonNegativeInteger} offsetA - starting index for A (in complex elements)
* @param {Complex128Array} x - complex input vector
* @param {integer} strideX - stride for x (in complex elements)
* @param {NonNegativeInteger} offsetX - starting index for x (in complex elements)
* @param {Complex128} beta - complex scalar
* @param {Complex128Array} y - complex input/output vector
* @param {integer} strideY - stride for y (in complex elements)
* @param {NonNegativeInteger} offsetY - starting index for y (in complex elements)
* @returns {Complex128Array} `y`
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
	var tmpA;
	var tmpB;
	var sa1;
	var sa2;
	var sx;
	var sy;
	var ix;
	var iy;
	var jx;
	var jy;
	var ai;
	var oA;
	var oX;
	var oY;
	var Av;
	var xv;
	var yv;
	var i;
	var j;

	if ( M === 0 || N === 0 ) {
		return y;
	}

	tmpA = complexParts( alpha );
	alphaR = tmpA[ 0 ];
	alphaI = tmpA[ 1 ];
	tmpB = complexParts( beta );
	betaR = tmpB[ 0 ];
	betaI = tmpB[ 1 ];

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

	// Get Float64Array views and convert offsets
	tmpA = float64view( A, offsetA );
	Av = tmpA[ 0 ];
	oA = tmpA[ 1 ];
	tmpA = float64view( x, offsetX );
	xv = tmpA[ 0 ];
	oX = tmpA[ 1 ];
	tmpA = float64view( y, offsetY );
	yv = tmpA[ 0 ];
	oY = tmpA[ 1 ];

	// Matrix strides are in complex elements, so multiply by 2
	sa1 = strideA1 * 2;
	sa2 = strideA2 * 2;

	// Vector strides in complex elements, multiply by 2
	sx = strideX * 2;
	sy = strideY * 2;

	// First form y := beta*y
	if ( betaR !== 1.0 || betaI !== 0.0 ) {
		iy = oY;
		if ( betaR === 0.0 && betaI === 0.0 ) {
			for ( i = 0; i < leny; i++ ) {
				yv[ iy ] = 0.0;
				yv[ iy + 1 ] = 0.0;
				iy += sy;
			}
		} else {
			for ( i = 0; i < leny; i++ ) {
				tempR = betaR * yv[ iy ] - betaI * yv[ iy + 1 ];
				tempI = betaR * yv[ iy + 1 ] + betaI * yv[ iy ];
				yv[ iy ] = tempR;
				yv[ iy + 1 ] = tempI;
				iy += sy;
			}
		}
	}

	if ( alphaR === 0.0 && alphaI === 0.0 ) {
		return y;
	}

	if ( noTrans ) {
		// Form y := alpha*A*x + y
		jx = oX;
		for ( j = 0; j < N; j++ ) {
			// temp = alpha * x[jx]
			tempR = alphaR * xv[ jx ] - alphaI * xv[ jx + 1 ];
			tempI = alphaR * xv[ jx + 1 ] + alphaI * xv[ jx ];
			iy = oY;
			ai = oA + j * sa2;
			for ( i = 0; i < M; i++ ) {
				aijR = Av[ ai ];
				aijI = Av[ ai + 1 ];
				// y[iy] += temp * A[i,j]
				yv[ iy ] += tempR * aijR - tempI * aijI;
				yv[ iy + 1 ] += tempR * aijI + tempI * aijR;
				iy += sy;
				ai += sa1;
			}
			jx += sx;
		}
	} else {
		// Form y := alpha*A**T*x + y  or  y := alpha*A**H*x + y
		jy = oY;
		for ( j = 0; j < N; j++ ) {
			tempR = 0.0;
			tempI = 0.0;
			ix = oX;
			ai = oA + j * sa2;
			if ( noConj ) {
				// Transpose (no conjugate)
				for ( i = 0; i < M; i++ ) {
					aijR = Av[ ai ];
					aijI = Av[ ai + 1 ];
					// temp += A[i,j] * x[ix]
					tempR += aijR * xv[ ix ] - aijI * xv[ ix + 1 ];
					tempI += aijR * xv[ ix + 1 ] + aijI * xv[ ix ];
					ix += sx;
					ai += sa1;
				}
			} else {
				// Conjugate transpose
				for ( i = 0; i < M; i++ ) {
					aijR = Av[ ai ];
					aijI = -Av[ ai + 1 ]; // conjugate
					// temp += conj(A[i,j]) * x[ix]
					tempR += aijR * xv[ ix ] - aijI * xv[ ix + 1 ];
					tempI += aijR * xv[ ix + 1 ] + aijI * xv[ ix ];
					ix += sx;
					ai += sa1;
				}
			}
			// y[jy] += alpha * temp
			yv[ jy ] += alphaR * tempR - alphaI * tempI;
			yv[ jy + 1 ] += alphaR * tempI + alphaI * tempR;
			jy += sy;
		}
	}
	return y;
}


// EXPORTS //

module.exports = zgemv;
