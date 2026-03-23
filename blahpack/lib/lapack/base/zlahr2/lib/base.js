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

/* eslint-disable max-len, max-params */
'use strict';
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zaxpy = require( '../../../../blas/base/zaxpy/lib/base.js' );
var zcopy = require( '../../../../blas/base/zcopy/lib/base.js' );
var zgemm = require( '../../../../blas/base/zgemm/lib/base.js' );
var zgemv = require( '../../../../blas/base/zgemv/lib/base.js' );
var zlacpy = require( '../../zlacpy/lib/base.js' );
var zlacgv = require( '../../zlacgv/lib/base.js' );
var zlarfg = require( '../../zlarfg/lib/base.js' );
var zscal = require( '../../../../blas/base/zscal/lib/base.js' );
var ztrmm = require( '../../../../blas/base/ztrmm/lib/base.js' );
var ztrmv = require( '../../../../blas/base/ztrmv/lib/base.js' );
var ZERO = new Complex128( 0.0, 0.0 );
var ONE = new Complex128( 1.0, 0.0 );
var NEGONE = new Complex128( -1.0, 0.0 );
function zlahr2( N, K, nb, A, strideA1, strideA2, offsetA, tau, strideTAU, offsetTAU, T, strideT1, strideT2, offsetT, Y, strideY1, strideY2, offsetY ) {
	var negTau, tauv, eiR, eiI, oTau, av, sa1, sa2, oA, oE, tv, oTi, i;
	if ( N <= 1 ) { return; }
	av = reinterpret( A, 0 );
	sa1 = strideA1 * 2; sa2 = strideA2 * 2; oA = offsetA * 2;
	tauv = reinterpret( tau, 0 );
	eiR = 0.0; eiI = 0.0;
	for ( i = 0; i < nb; i++ ) {
		if ( i > 0 ) {
			zlacgv( i, A, strideA2, offsetA + ( K + i - 1 ) * strideA1 );
			zgemv( 'no-transpose', N - K, i, NEGONE, Y, strideY1, strideY2, offsetY + K * strideY1, A, strideA2, offsetA + ( K + i - 1 ) * strideA1, ONE, A, strideA1, offsetA + K * strideA1 + i * strideA2 );
			zlacgv( i, A, strideA2, offsetA + ( K + i - 1 ) * strideA1 );
			zcopy( i, A, strideA1, offsetA + K * strideA1 + i * strideA2, T, strideT1, offsetT + ( nb - 1 ) * strideT2 );
			ztrmv( 'lower', 'conjugate-transpose', 'unit', i, A, strideA1, strideA2, offsetA + K * strideA1, T, strideT1, offsetT + ( nb - 1 ) * strideT2 );
			zgemv( 'conjugate-transpose', N - K - i, i, ONE, A, strideA1, strideA2, offsetA + ( K + i ) * strideA1, A, strideA1, offsetA + ( K + i ) * strideA1 + i * strideA2, ONE, T, strideT1, offsetT + ( nb - 1 ) * strideT2 );
			ztrmv( 'upper', 'conjugate-transpose', 'non-unit', i, T, strideT1, strideT2, offsetT, T, strideT1, offsetT + ( nb - 1 ) * strideT2 );
			zgemv( 'no-transpose', N - K - i, i, NEGONE, A, strideA1, strideA2, offsetA + ( K + i ) * strideA1, T, strideT1, offsetT + ( nb - 1 ) * strideT2, ONE, A, strideA1, offsetA + ( K + i ) * strideA1 + i * strideA2 );
			ztrmv( 'lower', 'no-transpose', 'unit', i, A, strideA1, strideA2, offsetA + K * strideA1, T, strideT1, offsetT + ( nb - 1 ) * strideT2 );
			zaxpy( i, NEGONE, T, strideT1, offsetT + ( nb - 1 ) * strideT2, A, strideA1, offsetA + K * strideA1 + i * strideA2 );
			oE = oA + ( K + i - 1 ) * sa1 + ( i - 1 ) * sa2;
			av[ oE ] = eiR; av[ oE + 1 ] = eiI;
		}
		zlarfg( N - K - i, A, offsetA + ( K + i ) * strideA1 + i * strideA2, A, strideA1, offsetA + Math.min( K + i + 1, N - 1 ) * strideA1 + i * strideA2, tau, offsetTAU + i * strideTAU );
		oE = oA + ( K + i ) * sa1 + i * sa2;
		eiR = av[ oE ]; eiI = av[ oE + 1 ];
		av[ oE ] = 1.0; av[ oE + 1 ] = 0.0;
		zgemv( 'no-transpose', N - K, N - K - i, ONE, A, strideA1, strideA2, offsetA + K * strideA1 + ( i + 1 ) * strideA2, A, strideA1, offsetA + ( K + i ) * strideA1 + i * strideA2, ZERO, Y, strideY1, offsetY + K * strideY1 + i * strideY2 );
		zgemv( 'conjugate-transpose', N - K - i, i, ONE, A, strideA1, strideA2, offsetA + ( K + i ) * strideA1, A, strideA1, offsetA + ( K + i ) * strideA1 + i * strideA2, ZERO, T, strideT1, offsetT + i * strideT2 );
		zgemv( 'no-transpose', N - K, i, NEGONE, Y, strideY1, strideY2, offsetY + K * strideY1, T, strideT1, offsetT + i * strideT2, ONE, Y, strideY1, offsetY + K * strideY1 + i * strideY2 );
		oTau = ( offsetTAU + i * strideTAU ) * 2;
		zscal( N - K, tau.get( offsetTAU + i * strideTAU ), Y, strideY1, offsetY + K * strideY1 + i * strideY2 );
		negTau = new Complex128( -tauv[ oTau ], -tauv[ oTau + 1 ] );
		zscal( i, negTau, T, strideT1, offsetT + i * strideT2 );
		ztrmv( 'upper', 'no-transpose', 'non-unit', i, T, strideT1, strideT2, offsetT, T, strideT1, offsetT + i * strideT2 );
		tv = reinterpret( T, 0 );
		oTi = ( offsetT + i * strideT1 + i * strideT2 ) * 2;
		tv[ oTi ] = tauv[ oTau ]; tv[ oTi + 1 ] = tauv[ oTau + 1 ];
	}
	oE = oA + ( K + nb - 1 ) * sa1 + ( nb - 1 ) * sa2;
	av[ oE ] = eiR; av[ oE + 1 ] = eiI;
	zlacpy( 'all', K, nb, A, strideA1, strideA2, offsetA + 1 * strideA2, Y, strideY1, strideY2, offsetY );
	ztrmm( 'right', 'lower', 'no-transpose', 'unit', K, nb, ONE, A, strideA1, strideA2, offsetA + K * strideA1, Y, strideY1, strideY2, offsetY );
	if ( N > K + nb ) {
		zgemm( 'no-transpose', 'no-transpose', K, nb, N - K - nb, ONE, A, strideA1, strideA2, offsetA + ( 1 + nb ) * strideA2, A, strideA1, strideA2, offsetA + ( K + nb ) * strideA1, ONE, Y, strideY1, strideY2, offsetY );
	}
	ztrmm( 'right', 'upper', 'no-transpose', 'non-unit', K, nb, ONE, T, strideT1, strideT2, offsetT, Y, strideY1, strideY2, offsetY );
}
module.exports = zlahr2;
