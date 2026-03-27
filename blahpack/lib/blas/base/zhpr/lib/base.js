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

/* eslint-disable max-len, max-params, max-statements */

'use strict';

// MAIN //

/**
* Performs the Hermitian packed rank-1 update `A := alpha*x*x**H + A`.
*
* `alpha` is a real scalar, `x` is an `N`-element complex vector, and `A` is
* an `N` by `N` Hermitian matrix supplied in packed form as a Float64Array
* with interleaved real and imaginary parts.
*
* @private
* @param {string} uplo - specifies whether upper or lower triangle is stored
* @param {NonNegativeInteger} N - order of the matrix `A`
* @param {number} alpha - real scalar constant
* @param {Float64Array} x - input vector (interleaved re/im pairs)
* @param {integer} strideX - stride length for `x` (in Float64 elements)
* @param {NonNegativeInteger} offsetX - starting index for `x` (in Float64 elements)
* @param {Float64Array} AP - packed Hermitian matrix (interleaved re/im pairs)
* @param {integer} strideAP - stride length for `AP` (in Float64 elements, typically 2)
* @param {NonNegativeInteger} offsetAP - starting index for `AP` (in Float64 elements)
* @returns {Float64Array} `AP`
*/
function zhpr( uplo, N, alpha, x, strideX, offsetX, AP, strideAP, offsetAP ) {
	var tempR;
	var tempI;
	var xjR;
	var xjI;
	var xiR;
	var xiI;
	var kk;
	var ix;
	var jx;
	var k;
	var i;
	var j;

	// Quick return if possible:
	if ( N === 0 || alpha === 0.0 ) {
		return AP;
	}

	jx = offsetX;
	kk = offsetAP;

	if ( uplo === 'upper' ) {
		// Form A when upper triangle is stored in AP:
		for ( j = 0; j < N; j += 1 ) {
			xjR = x[ jx ];
			xjI = x[ jx + 1 ];
			if ( xjR !== 0.0 || xjI !== 0.0 ) {
				// Temp = alpha * conj(x[j]):
				tempR = alpha * xjR;
				tempI = -( alpha * xjI );

				// Off-diagonal elements: AP[k] += x[i] * temp
				ix = offsetX;
				k = kk;
				for ( i = 0; i < j; i += 1 ) {
					xiR = x[ ix ];
					xiI = x[ ix + 1 ];

					// AP[k] += x[i] * temp (complex multiply):
					AP[ k ] += ( xiR * tempR ) - ( xiI * tempI );
					AP[ k + 1 ] += ( xiR * tempI ) + ( xiI * tempR );
					ix += strideX;
					k += strideAP;
				}

				// Diagonal: A(j,j) = real(A(j,j)) + real(x[j] * temp)
				AP[ k ] += ( xjR * tempR ) - ( xjI * tempI );
				AP[ k + 1 ] = 0.0;
			} else {
				// Force diagonal to real when x[j] == 0:
				k = kk + ( j * strideAP );
				AP[ k + 1 ] = 0.0;
			}
			jx += strideX;
			kk += ( j + 1 ) * strideAP;
		}
	} else {
		// Form A when lower triangle is stored in AP:
		for ( j = 0; j < N; j += 1 ) {
			xjR = x[ jx ];
			xjI = x[ jx + 1 ];
			if ( xjR !== 0.0 || xjI !== 0.0 ) {
				// Temp = alpha * conj(x[j]):
				tempR = alpha * xjR;
				tempI = -( alpha * xjI );

				// Diagonal: A(j,j) = real(A(j,j)) + real(temp * x[j])
				AP[ kk ] += ( xjR * tempR ) - ( xjI * tempI );
				AP[ kk + 1 ] = 0.0;

				// Off-diagonal elements below diagonal:
				ix = jx + strideX;
				k = kk + strideAP;
				for ( i = j + 1; i < N; i += 1 ) {
					xiR = x[ ix ];
					xiI = x[ ix + 1 ];

					// AP[k] += x[i] * temp (complex multiply):
					AP[ k ] += ( xiR * tempR ) - ( xiI * tempI );
					AP[ k + 1 ] += ( xiR * tempI ) + ( xiI * tempR );
					ix += strideX;
					k += strideAP;
				}
			} else {
				// Force diagonal to real when x[j] == 0:
				AP[ kk + 1 ] = 0.0;
			}
			jx += strideX;
			kk += ( N - j ) * strideAP;
		}
	}
	return AP;
}


// EXPORTS //

module.exports = zhpr;
