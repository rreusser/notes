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
* Perform one of the matrix-vector operations
*   x := A*x,  or  x := A**T*x,  or  x := A**H*x,
* where x is an N element complex vector and A is an N by N unit, or
* non-unit, upper or lower triangular complex matrix.
*
* Complex elements are stored as interleaved real/imaginary pairs.
* Element k of x has real part at `offsetX + 2*k*strideX` and
* imaginary part at `offsetX + 2*k*strideX + 1`.
*
* @private
* @param {string} uplo - 'U' or 'L'
* @param {string} trans - 'N', 'T', or 'C'
* @param {string} diag - 'U' or 'N'
* @param {NonNegativeInteger} N - order of the matrix
* @param {Float64Array} A - triangular matrix (interleaved complex)
* @param {integer} strideA1 - stride of the first dimension of `A` (complex elements)
* @param {integer} strideA2 - stride of the second dimension of `A` (complex elements)
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} x - vector (interleaved complex)
* @param {integer} strideX - stride for `x` (complex elements)
* @param {NonNegativeInteger} offsetX - starting index for `x`
* @returns {Float64Array} `x`
*/
function ztrmv( uplo, trans, diag, N, A, strideA1, strideA2, offsetA, x, strideX, offsetX ) { // eslint-disable-line max-len, max-params
	var noconj;
	var nounit;
	var upper;
	var sa1;
	var sa2;
	var sx;
	var ix;
	var jx;
	var ia;
	var tr;
	var ti;
	var ar;
	var ai;
	var xr;
	var xi;
	var i;
	var j;

	if ( N <= 0 ) {
		return x;
	}

	upper = ( uplo === 'U' || uplo === 'u' );
	noconj = ( trans === 'T' || trans === 't' );
	nounit = ( diag === 'N' || diag === 'n' );

	// Convert strides from complex-element units to double units
	sx = strideX * 2;
	sa1 = strideA1 * 2;
	sa2 = strideA2 * 2;

	if ( trans === 'N' || trans === 'n' ) {
		// Form x := A*x
		if ( upper ) {
			jx = offsetX;
			for ( j = 0; j < N; j++ ) {
				xr = x[ jx ];
				xi = x[ jx + 1 ];
				if ( xr !== 0.0 || xi !== 0.0 ) {
					ix = offsetX;
					for ( i = 0; i < j; i++ ) {
						ia = offsetA + i * sa1 + j * sa2;
						ar = A[ ia ];
						ai = A[ ia + 1 ];
						x[ ix ] += xr * ar - xi * ai;
						x[ ix + 1 ] += xr * ai + xi * ar;
						ix += sx;
					}
					if ( nounit ) {
						ia = offsetA + j * sa1 + j * sa2;
						ar = A[ ia ];
						ai = A[ ia + 1 ];
						tr = xr * ar - xi * ai;
						ti = xr * ai + xi * ar;
						x[ jx ] = tr;
						x[ jx + 1 ] = ti;
					}
				}
				jx += sx;
			}
		} else {
			// Lower triangular
			jx = offsetX + ( N - 1 ) * sx;
			for ( j = N - 1; j >= 0; j-- ) {
				xr = x[ jx ];
				xi = x[ jx + 1 ];
				if ( xr !== 0.0 || xi !== 0.0 ) {
					ix = offsetX + ( N - 1 ) * sx;
					for ( i = N - 1; i > j; i-- ) {
						ia = offsetA + i * sa1 + j * sa2;
						ar = A[ ia ];
						ai = A[ ia + 1 ];
						x[ ix ] += xr * ar - xi * ai;
						x[ ix + 1 ] += xr * ai + xi * ar;
						ix -= sx;
					}
					if ( nounit ) {
						ia = offsetA + j * sa1 + j * sa2;
						ar = A[ ia ];
						ai = A[ ia + 1 ];
						tr = xr * ar - xi * ai;
						ti = xr * ai + xi * ar;
						x[ jx ] = tr;
						x[ jx + 1 ] = ti;
					}
				}
				jx -= sx;
			}
		}
	} else {
		// Form x := A**T*x or x := A**H*x
		if ( upper ) {
			jx = offsetX + ( N - 1 ) * sx;
			for ( j = N - 1; j >= 0; j-- ) {
				tr = x[ jx ];
				ti = x[ jx + 1 ];
				if ( noconj ) {
					// Transpose (no conjugate)
					if ( nounit ) {
						ia = offsetA + j * sa1 + j * sa2;
						ar = A[ ia ];
						ai = A[ ia + 1 ];
						xr = tr * ar - ti * ai;
						xi = tr * ai + ti * ar;
						tr = xr;
						ti = xi;
					}
					ix = offsetX + ( j - 1 ) * sx;
					for ( i = j - 1; i >= 0; i-- ) {
						ia = offsetA + i * sa1 + j * sa2;
						ar = A[ ia ];
						ai = A[ ia + 1 ];
						tr += ar * x[ ix ] - ai * x[ ix + 1 ];
						ti += ar * x[ ix + 1 ] + ai * x[ ix ];
						ix -= sx;
					}
				} else {
					// Conjugate transpose
					if ( nounit ) {
						ia = offsetA + j * sa1 + j * sa2;
						ar = A[ ia ];
						ai = -A[ ia + 1 ];
						xr = tr * ar - ti * ai;
						xi = tr * ai + ti * ar;
						tr = xr;
						ti = xi;
					}
					ix = offsetX + ( j - 1 ) * sx;
					for ( i = j - 1; i >= 0; i-- ) {
						ia = offsetA + i * sa1 + j * sa2;
						ar = A[ ia ];
						ai = -A[ ia + 1 ];
						tr += ar * x[ ix ] - ai * x[ ix + 1 ];
						ti += ar * x[ ix + 1 ] + ai * x[ ix ];
						ix -= sx;
					}
				}
				x[ jx ] = tr;
				x[ jx + 1 ] = ti;
				jx -= sx;
			}
		} else {
			// Lower triangular, transpose/conjugate-transpose
			jx = offsetX;
			for ( j = 0; j < N; j++ ) {
				tr = x[ jx ];
				ti = x[ jx + 1 ];
				if ( noconj ) {
					if ( nounit ) {
						ia = offsetA + j * sa1 + j * sa2;
						ar = A[ ia ];
						ai = A[ ia + 1 ];
						xr = tr * ar - ti * ai;
						xi = tr * ai + ti * ar;
						tr = xr;
						ti = xi;
					}
					ix = offsetX + ( j + 1 ) * sx;
					for ( i = j + 1; i < N; i++ ) {
						ia = offsetA + i * sa1 + j * sa2;
						ar = A[ ia ];
						ai = A[ ia + 1 ];
						tr += ar * x[ ix ] - ai * x[ ix + 1 ];
						ti += ar * x[ ix + 1 ] + ai * x[ ix ];
						ix += sx;
					}
				} else {
					// Conjugate transpose
					if ( nounit ) {
						ia = offsetA + j * sa1 + j * sa2;
						ar = A[ ia ];
						ai = -A[ ia + 1 ];
						xr = tr * ar - ti * ai;
						xi = tr * ai + ti * ar;
						tr = xr;
						ti = xi;
					}
					ix = offsetX + ( j + 1 ) * sx;
					for ( i = j + 1; i < N; i++ ) {
						ia = offsetA + i * sa1 + j * sa2;
						ar = A[ ia ];
						ai = -A[ ia + 1 ];
						tr += ar * x[ ix ] - ai * x[ ix + 1 ];
						ti += ar * x[ ix + 1 ] + ai * x[ ix ];
						ix += sx;
					}
				}
				x[ jx ] = tr;
				x[ jx + 1 ] = ti;
				jx += sx;
			}
		}
	}
	return x;
}


// EXPORTS //

module.exports = ztrmv;
