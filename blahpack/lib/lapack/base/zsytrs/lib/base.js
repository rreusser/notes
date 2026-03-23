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
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var zgemv = require( '../../../../blas/base/zgemv/lib/base.js' );
var zgeru = require( '../../../../blas/base/zgeru/lib/base.js' );
var zscal = require( '../../../../blas/base/zscal/lib/base.js' );
var zswap = require( '../../../../blas/base/zswap/lib/base.js' );
var CONE = new Complex128( 1.0, 0.0 );
var NCONE = new Complex128( -1.0, 0.0 );
var _cdR = 0.0, _cdI = 0.0;
function cDiv( ar, ai, br, bi ) { var r, d; if ( Math.abs( bi ) <= Math.abs( br ) ) { r = bi / br; d = br + bi * r; _cdR = ( ar + ai * r ) / d; _cdI = ( ai - ar * r ) / d; } else { r = br / bi; d = bi + br * r; _cdR = ( ar * r + ai ) / d; _cdI = ( ai * r - ar ) / d; } }
function zsytrs( uplo, N, nrhs, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV, B, strideB1, strideB2, offsetB ) {
	var denomR, denomI, akm1kR, akm1kI, akm1R, akm1I, bkm1R, bkm1I;
	var sa1, sa2, sb1, sb2, Av, Bv, akR, akI, bkR, bkI;
	var kp, tr, ti, tR, tI, k, j, p1, p2;
	sa1 = strideA1 * 2; sa2 = strideA2 * 2; sb1 = strideB1 * 2; sb2 = strideB2 * 2;
	Av = reinterpret( A, 0 ); Bv = reinterpret( B, 0 );
	if ( N === 0 || nrhs === 0 ) return 0;
	if ( uplo === 'upper' ) {
		k = N - 1;
		while ( k >= 0 ) {
			if ( IPIV[ offsetIPIV + k * strideIPIV ] >= 0 ) {
				kp = IPIV[ offsetIPIV + k * strideIPIV ];
				if ( kp !== k ) zswap( nrhs, B, strideB2, offsetB + k * strideB1, B, strideB2, offsetB + kp * strideB1 );
				if ( k > 0 ) zgeru( k, nrhs, NCONE, A, strideA1, offsetA + k * strideA2, B, strideB2, offsetB + k * strideB1, B, strideB1, strideB2, offsetB );
				p1 = offsetA * 2 + k * sa1 + k * sa2; tr = Av[ p1 ]; ti = Av[ p1 + 1 ];
				cDiv( 1.0, 0.0, tr, ti ); zscal( nrhs, new Complex128( _cdR, _cdI ), B, strideB2, offsetB + k * strideB1 );
				k -= 1;
			} else {
				kp = ~IPIV[ offsetIPIV + k * strideIPIV ];
				if ( kp !== k - 1 ) zswap( nrhs, B, strideB2, offsetB + ( k - 1 ) * strideB1, B, strideB2, offsetB + kp * strideB1 );
				if ( k > 1 ) { zgeru( k - 1, nrhs, NCONE, A, strideA1, offsetA + k * strideA2, B, strideB2, offsetB + k * strideB1, B, strideB1, strideB2, offsetB ); zgeru( k - 1, nrhs, NCONE, A, strideA1, offsetA + ( k - 1 ) * strideA2, B, strideB2, offsetB + ( k - 1 ) * strideB1, B, strideB1, strideB2, offsetB ); }
				p1 = offsetA * 2 + ( k - 1 ) * sa1 + k * sa2; akm1kR = Av[ p1 ]; akm1kI = Av[ p1 + 1 ];
				p2 = offsetA * 2 + ( k - 1 ) * sa1 + ( k - 1 ) * sa2; cDiv( Av[ p2 ], Av[ p2 + 1 ], akm1kR, akm1kI ); akm1R = _cdR; akm1I = _cdI;
				p2 = offsetA * 2 + k * sa1 + k * sa2; cDiv( Av[ p2 ], Av[ p2 + 1 ], akm1kR, akm1kI ); akR = _cdR; akI = _cdI;
				denomR = akm1R * akR - akm1I * akI - 1.0; denomI = akm1R * akI + akm1I * akR;
				for ( j = 0; j < nrhs; j++ ) { p1 = offsetB * 2 + ( k - 1 ) * sb1 + j * sb2; cDiv( Bv[ p1 ], Bv[ p1 + 1 ], akm1kR, akm1kI ); bkm1R = _cdR; bkm1I = _cdI; p2 = offsetB * 2 + k * sb1 + j * sb2; cDiv( Bv[ p2 ], Bv[ p2 + 1 ], akm1kR, akm1kI ); bkR = _cdR; bkI = _cdI; tr = akR * bkm1R - akI * bkm1I - bkR; ti = akR * bkm1I + akI * bkm1R - bkI; cDiv( tr, ti, denomR, denomI ); Bv[ p1 ] = _cdR; Bv[ p1 + 1 ] = _cdI; tr = akm1R * bkR - akm1I * bkI - bkm1R; ti = akm1R * bkI + akm1I * bkR - bkm1I; cDiv( tr, ti, denomR, denomI ); Bv[ p2 ] = _cdR; Bv[ p2 + 1 ] = _cdI; }
				k -= 2;
			}
		}
		k = 0;
		while ( k < N ) {
			if ( IPIV[ offsetIPIV + k * strideIPIV ] >= 0 ) {
				if ( k > 0 ) zgemv( 'transpose', k, nrhs, NCONE, B, strideB1, strideB2, offsetB, A, strideA1, offsetA + k * strideA2, CONE, B, strideB2, offsetB + k * strideB1 );
				kp = IPIV[ offsetIPIV + k * strideIPIV ]; if ( kp !== k ) zswap( nrhs, B, strideB2, offsetB + k * strideB1, B, strideB2, offsetB + kp * strideB1 );
				k += 1;
			} else {
				if ( k > 0 ) { zgemv( 'transpose', k, nrhs, NCONE, B, strideB1, strideB2, offsetB, A, strideA1, offsetA + k * strideA2, CONE, B, strideB2, offsetB + k * strideB1 ); zgemv( 'transpose', k, nrhs, NCONE, B, strideB1, strideB2, offsetB, A, strideA1, offsetA + ( k + 1 ) * strideA2, CONE, B, strideB2, offsetB + ( k + 1 ) * strideB1 ); }
				kp = ~IPIV[ offsetIPIV + k * strideIPIV ]; if ( kp !== k ) zswap( nrhs, B, strideB2, offsetB + k * strideB1, B, strideB2, offsetB + kp * strideB1 );
				k += 2;
			}
		}
	} else {
		k = 0;
		while ( k < N ) {
			if ( IPIV[ offsetIPIV + k * strideIPIV ] >= 0 ) {
				kp = IPIV[ offsetIPIV + k * strideIPIV ]; if ( kp !== k ) zswap( nrhs, B, strideB2, offsetB + k * strideB1, B, strideB2, offsetB + kp * strideB1 );
				if ( k < N - 1 ) zgeru( N - k - 1, nrhs, NCONE, A, strideA1, offsetA + ( k + 1 ) * strideA1 + k * strideA2, B, strideB2, offsetB + k * strideB1, B, strideB1, strideB2, offsetB + ( k + 1 ) * strideB1 );
				p1 = offsetA * 2 + k * sa1 + k * sa2; cDiv( 1.0, 0.0, Av[ p1 ], Av[ p1 + 1 ] ); zscal( nrhs, new Complex128( _cdR, _cdI ), B, strideB2, offsetB + k * strideB1 );
				k += 1;
			} else {
				kp = ~IPIV[ offsetIPIV + k * strideIPIV ]; if ( kp !== k + 1 ) zswap( nrhs, B, strideB2, offsetB + ( k + 1 ) * strideB1, B, strideB2, offsetB + kp * strideB1 );
				if ( k < N - 2 ) { zgeru( N - k - 2, nrhs, NCONE, A, strideA1, offsetA + ( k + 2 ) * strideA1 + k * strideA2, B, strideB2, offsetB + k * strideB1, B, strideB1, strideB2, offsetB + ( k + 2 ) * strideB1 ); zgeru( N - k - 2, nrhs, NCONE, A, strideA1, offsetA + ( k + 2 ) * strideA1 + ( k + 1 ) * strideA2, B, strideB2, offsetB + ( k + 1 ) * strideB1, B, strideB1, strideB2, offsetB + ( k + 2 ) * strideB1 ); }
				p1 = offsetA * 2 + ( k + 1 ) * sa1 + k * sa2; akm1kR = Av[ p1 ]; akm1kI = Av[ p1 + 1 ];
				p2 = offsetA * 2 + k * sa1 + k * sa2; cDiv( Av[ p2 ], Av[ p2 + 1 ], akm1kR, akm1kI ); akm1R = _cdR; akm1I = _cdI;
				p2 = offsetA * 2 + ( k + 1 ) * sa1 + ( k + 1 ) * sa2; cDiv( Av[ p2 ], Av[ p2 + 1 ], akm1kR, akm1kI ); akR = _cdR; akI = _cdI;
				denomR = akm1R * akR - akm1I * akI - 1.0; denomI = akm1R * akI + akm1I * akR;
				for ( j = 0; j < nrhs; j++ ) { p1 = offsetB * 2 + k * sb1 + j * sb2; cDiv( Bv[ p1 ], Bv[ p1 + 1 ], akm1kR, akm1kI ); bkm1R = _cdR; bkm1I = _cdI; p2 = offsetB * 2 + ( k + 1 ) * sb1 + j * sb2; cDiv( Bv[ p2 ], Bv[ p2 + 1 ], akm1kR, akm1kI ); bkR = _cdR; bkI = _cdI; tr = akR * bkm1R - akI * bkm1I - bkR; ti = akR * bkm1I + akI * bkm1R - bkI; cDiv( tr, ti, denomR, denomI ); Bv[ p1 ] = _cdR; Bv[ p1 + 1 ] = _cdI; tr = akm1R * bkR - akm1I * bkI - bkm1R; ti = akm1R * bkI + akm1I * bkR - bkm1I; cDiv( tr, ti, denomR, denomI ); Bv[ p2 ] = _cdR; Bv[ p2 + 1 ] = _cdI; }
				k += 2;
			}
		}
		k = N - 1;
		while ( k >= 0 ) {
			if ( IPIV[ offsetIPIV + k * strideIPIV ] >= 0 ) {
				if ( k < N - 1 ) zgemv( 'transpose', N - k - 1, nrhs, NCONE, B, strideB1, strideB2, offsetB + ( k + 1 ) * strideB1, A, strideA1, offsetA + ( k + 1 ) * strideA1 + k * strideA2, CONE, B, strideB2, offsetB + k * strideB1 );
				kp = IPIV[ offsetIPIV + k * strideIPIV ]; if ( kp !== k ) zswap( nrhs, B, strideB2, offsetB + k * strideB1, B, strideB2, offsetB + kp * strideB1 );
				k -= 1;
			} else {
				if ( k < N - 1 ) { zgemv( 'transpose', N - k - 1, nrhs, NCONE, B, strideB1, strideB2, offsetB + ( k + 1 ) * strideB1, A, strideA1, offsetA + ( k + 1 ) * strideA1 + k * strideA2, CONE, B, strideB2, offsetB + k * strideB1 ); zgemv( 'transpose', N - k - 1, nrhs, NCONE, B, strideB1, strideB2, offsetB + ( k + 1 ) * strideB1, A, strideA1, offsetA + ( k + 1 ) * strideA1 + ( k - 1 ) * strideA2, CONE, B, strideB2, offsetB + ( k - 1 ) * strideB1 ); }
				kp = ~IPIV[ offsetIPIV + k * strideIPIV ]; if ( kp !== k ) zswap( nrhs, B, strideB2, offsetB + k * strideB1, B, strideB2, offsetB + kp * strideB1 );
				k -= 2;
			}
		}
	}
	return 0;
}
module.exports = zsytrs;
