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

var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );

// MAIN //

/**
* Perform one of the matrix-vector operations
*   x := A*x,  or  x := A**T*x,  or  x := A**H*x,
* where x is an N element complex vector and A is an N by N unit, or
* non-unit, upper or lower triangular complex matrix.
*
* @private
* @param {string} uplo - 'U' or 'L'
* @param {string} trans - 'N', 'T', or 'C'
* @param {string} diag - 'U' or 'N'
* @param {NonNegativeInteger} N - order of the matrix
* @param {Complex128Array} A - complex triangular matrix
* @param {integer} strideA1 - stride of the first dimension of `A` (complex elements)
* @param {integer} strideA2 - stride of the second dimension of `A` (complex elements)
* @param {NonNegativeInteger} offsetA - starting index for `A` (in complex elements)
* @param {Complex128Array} x - complex vector
* @param {integer} strideX - stride for `x` (complex elements)
* @param {NonNegativeInteger} offsetX - starting index for `x` (in complex elements)
* @returns {Complex128Array} `x`
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
	var oA;
	var oX;
	var Av;
	var xv;
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

	// Get Float64Array views
	Av = reinterpret( A, 0 );
	xv = reinterpret( x, 0 );

	// Convert offsets from complex elements to Float64
	oA = offsetA * 2;
	oX = offsetX * 2;

	// Convert strides from complex-element units to double units
	sx = strideX * 2;
	sa1 = strideA1 * 2;
	sa2 = strideA2 * 2;

	if ( trans === 'N' || trans === 'n' ) {
		// Form x := A*x
		if ( upper ) {
			jx = oX;
			for ( j = 0; j < N; j++ ) {
				xr = xv[ jx ];
				xi = xv[ jx + 1 ];
				if ( xr !== 0.0 || xi !== 0.0 ) {
					ix = oX;
					for ( i = 0; i < j; i++ ) {
						ia = oA + i * sa1 + j * sa2;
						ar = Av[ ia ];
						ai = Av[ ia + 1 ];
						xv[ ix ] += xr * ar - xi * ai;
						xv[ ix + 1 ] += xr * ai + xi * ar;
						ix += sx;
					}
					if ( nounit ) {
						ia = oA + j * sa1 + j * sa2;
						ar = Av[ ia ];
						ai = Av[ ia + 1 ];
						tr = xr * ar - xi * ai;
						ti = xr * ai + xi * ar;
						xv[ jx ] = tr;
						xv[ jx + 1 ] = ti;
					}
				}
				jx += sx;
			}
		} else {
			// Lower triangular
			jx = oX + ( N - 1 ) * sx;
			for ( j = N - 1; j >= 0; j-- ) {
				xr = xv[ jx ];
				xi = xv[ jx + 1 ];
				if ( xr !== 0.0 || xi !== 0.0 ) {
					ix = oX + ( N - 1 ) * sx;
					for ( i = N - 1; i > j; i-- ) {
						ia = oA + i * sa1 + j * sa2;
						ar = Av[ ia ];
						ai = Av[ ia + 1 ];
						xv[ ix ] += xr * ar - xi * ai;
						xv[ ix + 1 ] += xr * ai + xi * ar;
						ix -= sx;
					}
					if ( nounit ) {
						ia = oA + j * sa1 + j * sa2;
						ar = Av[ ia ];
						ai = Av[ ia + 1 ];
						tr = xr * ar - xi * ai;
						ti = xr * ai + xi * ar;
						xv[ jx ] = tr;
						xv[ jx + 1 ] = ti;
					}
				}
				jx -= sx;
			}
		}
	} else {
		// Form x := A**T*x or x := A**H*x
		if ( upper ) {
			jx = oX + ( N - 1 ) * sx;
			for ( j = N - 1; j >= 0; j-- ) {
				tr = xv[ jx ];
				ti = xv[ jx + 1 ];
				if ( noconj ) {
					// Transpose (no conjugate)
					if ( nounit ) {
						ia = oA + j * sa1 + j * sa2;
						ar = Av[ ia ];
						ai = Av[ ia + 1 ];
						xr = tr * ar - ti * ai;
						xi = tr * ai + ti * ar;
						tr = xr;
						ti = xi;
					}
					ix = oX + ( j - 1 ) * sx;
					for ( i = j - 1; i >= 0; i-- ) {
						ia = oA + i * sa1 + j * sa2;
						ar = Av[ ia ];
						ai = Av[ ia + 1 ];
						tr += ar * xv[ ix ] - ai * xv[ ix + 1 ];
						ti += ar * xv[ ix + 1 ] + ai * xv[ ix ];
						ix -= sx;
					}
				} else {
					// Conjugate transpose
					if ( nounit ) {
						ia = oA + j * sa1 + j * sa2;
						ar = Av[ ia ];
						ai = -Av[ ia + 1 ];
						xr = tr * ar - ti * ai;
						xi = tr * ai + ti * ar;
						tr = xr;
						ti = xi;
					}
					ix = oX + ( j - 1 ) * sx;
					for ( i = j - 1; i >= 0; i-- ) {
						ia = oA + i * sa1 + j * sa2;
						ar = Av[ ia ];
						ai = -Av[ ia + 1 ];
						tr += ar * xv[ ix ] - ai * xv[ ix + 1 ];
						ti += ar * xv[ ix + 1 ] + ai * xv[ ix ];
						ix -= sx;
					}
				}
				xv[ jx ] = tr;
				xv[ jx + 1 ] = ti;
				jx -= sx;
			}
		} else {
			// Lower triangular, transpose/conjugate-transpose
			jx = oX;
			for ( j = 0; j < N; j++ ) {
				tr = xv[ jx ];
				ti = xv[ jx + 1 ];
				if ( noconj ) {
					if ( nounit ) {
						ia = oA + j * sa1 + j * sa2;
						ar = Av[ ia ];
						ai = Av[ ia + 1 ];
						xr = tr * ar - ti * ai;
						xi = tr * ai + ti * ar;
						tr = xr;
						ti = xi;
					}
					ix = oX + ( j + 1 ) * sx;
					for ( i = j + 1; i < N; i++ ) {
						ia = oA + i * sa1 + j * sa2;
						ar = Av[ ia ];
						ai = Av[ ia + 1 ];
						tr += ar * xv[ ix ] - ai * xv[ ix + 1 ];
						ti += ar * xv[ ix + 1 ] + ai * xv[ ix ];
						ix += sx;
					}
				} else {
					// Conjugate transpose
					if ( nounit ) {
						ia = oA + j * sa1 + j * sa2;
						ar = Av[ ia ];
						ai = -Av[ ia + 1 ];
						xr = tr * ar - ti * ai;
						xi = tr * ai + ti * ar;
						tr = xr;
						ti = xi;
					}
					ix = oX + ( j + 1 ) * sx;
					for ( i = j + 1; i < N; i++ ) {
						ia = oA + i * sa1 + j * sa2;
						ar = Av[ ia ];
						ai = -Av[ ia + 1 ];
						tr += ar * xv[ ix ] - ai * xv[ ix + 1 ];
						ti += ar * xv[ ix + 1 ] + ai * xv[ ix ];
						ix += sx;
					}
				}
				xv[ jx ] = tr;
				xv[ jx + 1 ] = ti;
				jx += sx;
			}
		}
	}
	return x;
}


// EXPORTS //

module.exports = ztrmv;
