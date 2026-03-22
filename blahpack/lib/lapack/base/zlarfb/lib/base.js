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

var zcopy = require( '../../../../blas/base/zcopy/lib/base.js' );
var zgemm = require( '../../../../blas/base/zgemm/lib/base.js' );
var ztrmm = require( '../../../../blas/base/ztrmm/lib/base.js' );
var zlacgv = require( '../../zlacgv/lib/base.js' );

// VARIABLES //

var ONE = new Float64Array( [ 1.0, 0.0 ] );
var NEGONE = new Float64Array( [ -1.0, 0.0 ] );

// MAIN //

/**
* Apply a complex block reflector H or its conjugate-transpose H^H to a
* complex M-by-N matrix C, from either the left or the right.
*
* H = I - V * T * V^H  (for STOREV='C')
*
* Complex elements are stored as interleaved real/imaginary pairs.
* All strides are in complex-element units.
*
* Currently only STOREV='C' (columnwise) is supported.
*
* @private
* @param {string} side - 'L' or 'R'
* @param {string} trans - 'N' or 'C'
* @param {string} direct - 'F' or 'B'
* @param {string} storev - 'C' or 'R'
* @param {NonNegativeInteger} M - rows of C
* @param {NonNegativeInteger} N - columns of C
* @param {NonNegativeInteger} K - number of elementary reflectors
* @param {Float64Array} V - matrix of reflector vectors (interleaved complex)
* @param {integer} strideV1 - first dim stride of V (complex elements)
* @param {integer} strideV2 - second dim stride of V (complex elements)
* @param {NonNegativeInteger} offsetV - starting index for V
* @param {Float64Array} T - triangular factor (interleaved complex)
* @param {integer} strideT1 - first dim stride of T (complex elements)
* @param {integer} strideT2 - second dim stride of T (complex elements)
* @param {NonNegativeInteger} offsetT - starting index for T
* @param {Float64Array} C - matrix (interleaved complex), modified in-place
* @param {integer} strideC1 - first dim stride of C (complex elements)
* @param {integer} strideC2 - second dim stride of C (complex elements)
* @param {NonNegativeInteger} offsetC - starting index for C
* @param {Float64Array} WORK - workspace (interleaved complex)
* @param {integer} strideWORK1 - first dim stride of WORK (complex elements)
* @param {integer} strideWORK2 - second dim stride of WORK (complex elements)
* @param {NonNegativeInteger} offsetWORK - starting index for WORK
*/
function zlarfb( side, trans, direct, storev, M, N, K, V, strideV1, strideV2, offsetV, T, strideT1, strideT2, offsetT, C, strideC1, strideC2, offsetC, WORK, strideWORK1, strideWORK2, offsetWORK ) { // eslint-disable-line max-len, max-params
	var transt;
	var sw1;
	var sw2;
	var sc1;
	var sc2;
	var iw;
	var ic;
	var wr;
	var wi;
	var i;
	var j;

	if ( M <= 0 || N <= 0 ) {
		return;
	}

	sw1 = strideWORK1 * 2;
	sw2 = strideWORK2 * 2;
	sc1 = strideC1 * 2;
	sc2 = strideC2 * 2;

	if ( trans === 'N' || trans === 'n' ) {
		transt = 'C';
	} else {
		transt = 'N';
	}

	if ( storev === 'C' || storev === 'c' ) {
		if ( direct === 'F' || direct === 'f' ) {
			// V = (V1)  first K rows, V1 is unit lower triangular
			//     (V2)
			if ( side === 'L' || side === 'l' ) {
				// Form H*C or H^H*C where C = (C1)
				//                              (C2)
				// W := C^H * V = (C1^H*V1 + C2^H*V2)  stored in WORK
				// W := C1^H (copy rows of C1 into columns of WORK, conjugating)
				for ( j = 0; j < K; j++ ) {
					zcopy( N, C, strideC2, offsetC + j * sc1, WORK, strideWORK1, offsetWORK + j * sw2 );
					zlacgv( N, WORK, strideWORK1, offsetWORK + j * sw2 );
				}
				// W := W * V1
				ztrmm( 'R', 'L', 'N', 'U', N, K, ONE, V, strideV1, strideV2, offsetV, WORK, strideWORK1, strideWORK2, offsetWORK );
				if ( M > K ) {
					// W := W + C2^H * V2
					zgemm( 'C', 'N', N, K, M - K, ONE,
						C, strideC1, strideC2, offsetC + K * sc1,
						V, strideV1, strideV2, offsetV + K * strideV1 * 2,
						ONE, WORK, strideWORK1, strideWORK2, offsetWORK );
				}
				// W := W * T^H or W * T
				ztrmm( 'R', 'U', transt, 'N', N, K, ONE, T, strideT1, strideT2, offsetT, WORK, strideWORK1, strideWORK2, offsetWORK );
				// C := C - V * W^H
				if ( M > K ) {
					// C2 := C2 - V2 * W^H
					zgemm( 'N', 'C', M - K, N, K, NEGONE,
						V, strideV1, strideV2, offsetV + K * strideV1 * 2,
						WORK, strideWORK1, strideWORK2, offsetWORK,
						ONE, C, strideC1, strideC2, offsetC + K * sc1 );
				}
				// W := W * V1^H
				ztrmm( 'R', 'L', 'C', 'U', N, K, ONE, V, strideV1, strideV2, offsetV, WORK, strideWORK1, strideWORK2, offsetWORK );
				// C1 := C1 - W^H
				for ( j = 0; j < K; j++ ) {
					for ( i = 0; i < N; i++ ) {
						ic = offsetC + j * sc1 + i * sc2;
						iw = offsetWORK + i * sw1 + j * sw2;
						// C(j,i) -= conj(W(i,j))
						C[ ic ] -= WORK[ iw ];
						C[ ic + 1 ] -= ( -WORK[ iw + 1 ] );
					}
				}
			} else if ( side === 'R' || side === 'r' ) {
				// Form C*H or C*H^H where C = (C1 C2)
				// W := C * V = (C1*V1 + C2*V2)
				// W := C1
				for ( j = 0; j < K; j++ ) {
					zcopy( M, C, strideC1, offsetC + j * sc2, WORK, strideWORK1, offsetWORK + j * sw2 );
				}
				// W := W * V1
				ztrmm( 'R', 'L', 'N', 'U', M, K, ONE, V, strideV1, strideV2, offsetV, WORK, strideWORK1, strideWORK2, offsetWORK );
				if ( N > K ) {
					// W := W + C2 * V2
					zgemm( 'N', 'N', M, K, N - K, ONE,
						C, strideC1, strideC2, offsetC + K * sc2,
						V, strideV1, strideV2, offsetV + K * strideV1 * 2,
						ONE, WORK, strideWORK1, strideWORK2, offsetWORK );
				}
				// W := W * T or W * T^H
				ztrmm( 'R', 'U', trans, 'N', M, K, ONE, T, strideT1, strideT2, offsetT, WORK, strideWORK1, strideWORK2, offsetWORK );
				// C := C - W * V^H
				if ( N > K ) {
					// C2 := C2 - W * V2^H
					zgemm( 'N', 'C', M, N - K, K, NEGONE,
						WORK, strideWORK1, strideWORK2, offsetWORK,
						V, strideV1, strideV2, offsetV + K * strideV1 * 2,
						ONE, C, strideC1, strideC2, offsetC + K * sc2 );
				}
				// W := W * V1^H
				ztrmm( 'R', 'L', 'C', 'U', M, K, ONE, V, strideV1, strideV2, offsetV, WORK, strideWORK1, strideWORK2, offsetWORK );
				// C1 := C1 - W
				for ( j = 0; j < K; j++ ) {
					for ( i = 0; i < M; i++ ) {
						ic = offsetC + i * sc1 + j * sc2;
						iw = offsetWORK + i * sw1 + j * sw2;
						C[ ic ] -= WORK[ iw ];
						C[ ic + 1 ] -= WORK[ iw + 1 ];
					}
				}
			}
		} else {
			// Backward: V = (V1)
			//               (V2)  last K rows, V2 is unit upper triangular
			if ( side === 'L' || side === 'l' ) {
				// Form H*C or H^H*C where C = (C1)
				//                              (C2)
				// W := C^H * V = (C1^H*V1 + C2^H*V2)
				// W := C2^H
				for ( j = 0; j < K; j++ ) {
					zcopy( N, C, strideC2, offsetC + ( M - K + j ) * sc1, WORK, strideWORK1, offsetWORK + j * sw2 );
					zlacgv( N, WORK, strideWORK1, offsetWORK + j * sw2 );
				}
				// W := W * V2
				ztrmm( 'R', 'U', 'N', 'U', N, K, ONE, V, strideV1, strideV2, offsetV + ( M - K ) * strideV1 * 2, WORK, strideWORK1, strideWORK2, offsetWORK );
				if ( M > K ) {
					// W := W + C1^H * V1
					zgemm( 'C', 'N', N, K, M - K, ONE,
						C, strideC1, strideC2, offsetC,
						V, strideV1, strideV2, offsetV,
						ONE, WORK, strideWORK1, strideWORK2, offsetWORK );
				}
				// W := W * T^H or W * T
				ztrmm( 'R', 'L', transt, 'N', N, K, ONE, T, strideT1, strideT2, offsetT, WORK, strideWORK1, strideWORK2, offsetWORK );
				// C := C - V * W^H
				if ( M > K ) {
					// C1 := C1 - V1 * W^H
					zgemm( 'N', 'C', M - K, N, K, NEGONE,
						V, strideV1, strideV2, offsetV,
						WORK, strideWORK1, strideWORK2, offsetWORK,
						ONE, C, strideC1, strideC2, offsetC );
				}
				// W := W * V2^H
				ztrmm( 'R', 'U', 'C', 'U', N, K, ONE, V, strideV1, strideV2, offsetV + ( M - K ) * strideV1 * 2, WORK, strideWORK1, strideWORK2, offsetWORK );
				// C2 := C2 - W^H
				for ( j = 0; j < K; j++ ) {
					for ( i = 0; i < N; i++ ) {
						ic = offsetC + ( M - K + j ) * sc1 + i * sc2;
						iw = offsetWORK + i * sw1 + j * sw2;
						C[ ic ] -= WORK[ iw ];
						C[ ic + 1 ] -= ( -WORK[ iw + 1 ] );
					}
				}
			} else if ( side === 'R' || side === 'r' ) {
				// Form C*H or C*H^H where C = (C1 C2)
				// W := C * V = (C1*V1 + C2*V2)
				// W := C2
				for ( j = 0; j < K; j++ ) {
					zcopy( M, C, strideC1, offsetC + ( N - K + j ) * sc2, WORK, strideWORK1, offsetWORK + j * sw2 );
				}
				// W := W * V2
				ztrmm( 'R', 'U', 'N', 'U', M, K, ONE, V, strideV1, strideV2, offsetV + ( N - K ) * strideV1 * 2, WORK, strideWORK1, strideWORK2, offsetWORK );
				if ( N > K ) {
					// W := W + C1 * V1
					zgemm( 'N', 'N', M, K, N - K, ONE,
						C, strideC1, strideC2, offsetC,
						V, strideV1, strideV2, offsetV,
						ONE, WORK, strideWORK1, strideWORK2, offsetWORK );
				}
				// W := W * T or W * T^H
				ztrmm( 'R', 'L', trans, 'N', M, K, ONE, T, strideT1, strideT2, offsetT, WORK, strideWORK1, strideWORK2, offsetWORK );
				// C := C - W * V^H
				if ( N > K ) {
					// C1 := C1 - W * V1^H
					zgemm( 'N', 'C', M, N - K, K, NEGONE,
						WORK, strideWORK1, strideWORK2, offsetWORK,
						V, strideV1, strideV2, offsetV,
						ONE, C, strideC1, strideC2, offsetC );
				}
				// W := W * V2^H
				ztrmm( 'R', 'U', 'C', 'U', M, K, ONE, V, strideV1, strideV2, offsetV + ( N - K ) * strideV1 * 2, WORK, strideWORK1, strideWORK2, offsetWORK );
				// C2 := C2 - W
				for ( j = 0; j < K; j++ ) {
					for ( i = 0; i < M; i++ ) {
						ic = offsetC + i * sc1 + ( N - K + j ) * sc2;
						iw = offsetWORK + i * sw1 + j * sw2;
						C[ ic ] -= WORK[ iw ];
						C[ ic + 1 ] -= WORK[ iw + 1 ];
					}
				}
			}
		}
	} else {
		throw new Error( 'zlarfb: STOREV=R not yet implemented' );
	}
}


// EXPORTS //

module.exports = zlarfb;
