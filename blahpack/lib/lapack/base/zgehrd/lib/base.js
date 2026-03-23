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
var zgehd2 = require( '../../zgehd2/lib/base.js' );
var zgemm = require( '../../../../blas/base/zgemm/lib/base.js' );
var zlahr2 = require( '../../zlahr2/lib/base.js' );
var zlarfb = require( '../../zlarfb/lib/base.js' );
var ztrmm = require( '../../../../blas/base/ztrmm/lib/base.js' );
var NBMAX = 64;
var LDT = NBMAX + 1;
var ONE = new Complex128( 1.0, 0.0 );
var NEGONE = new Complex128( -1.0, 0.0 );
function zgehrd( N, ilo, ihi, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK, lwork ) {
	var LDWORK;
	var tauv;
	var eiR;
	var eiI;
	var IWT;
	var sa1;
	var sa2;
	var av;
	var oA;
	var oE;
	var NB;
	var NX;
	var NH;
	var IB;
	var i;
	var j;
	NB = 32;
	av = reinterpret( A, 0 ); sa1 = strideA1 * 2; sa2 = strideA2 * 2; oA = offsetA * 2;
	tauv = reinterpret( TAU, 0 );
	for ( i = 0; i < ilo - 1; i++ ) { tauv[ ( offsetTAU + i * strideTAU ) * 2 ] = 0.0; tauv[ ( offsetTAU + i * strideTAU ) * 2 + 1 ] = 0.0; }
	for ( i = Math.max( 0, ihi - 1 ); i < N - 1; i++ ) { tauv[ ( offsetTAU + i * strideTAU ) * 2 ] = 0.0; tauv[ ( offsetTAU + i * strideTAU ) * 2 + 1 ] = 0.0; }
	NH = ihi - ilo + 1;
	if ( NH <= 1 ) { return 0; }
	NX = Math.max( NB, NB ); LDWORK = N;
	if ( NB < 2 || NB >= NH ) {
		i = ilo;
	} else {
		IWT = N * NB;
		for ( i = ilo; i <= ihi - 1 - NX; i += NB ) {
			IB = Math.min( NB, ihi - i );
			zlahr2( ihi, i, IB, A, strideA1, strideA2, offsetA + ( i - 1 ) * strideA2, TAU, strideTAU, offsetTAU + ( i - 1 ) * strideTAU, WORK, 1, LDT, offsetWORK + IWT, WORK, 1, LDWORK, offsetWORK );
			oE = oA + ( i + IB - 1 ) * sa1 + ( i + IB - 2 ) * sa2;
			eiR = av[ oE ]; eiI = av[ oE + 1 ]; av[ oE ] = 1.0; av[ oE + 1 ] = 0.0;
			zgemm( 'no-transpose', 'conjugate-transpose', ihi, ihi - i - IB + 1, IB, NEGONE, WORK, 1, LDWORK, offsetWORK, A, strideA1, strideA2, offsetA + ( i + IB - 1 ) * strideA1 + ( i - 1 ) * strideA2, ONE, A, strideA1, strideA2, offsetA + ( i + IB - 1 ) * strideA2 );
			av[ oE ] = eiR; av[ oE + 1 ] = eiI;
			ztrmm( 'right', 'lower', 'conjugate-transpose', 'unit', i, IB - 1, ONE, A, strideA1, strideA2, offsetA + i * strideA1 + ( i - 1 ) * strideA2, WORK, 1, LDWORK, offsetWORK );
			for ( j = 0; j < IB - 1; j++ ) { zaxpy( i, NEGONE, WORK, 1, offsetWORK + LDWORK * j, A, strideA1, offsetA + ( i + j ) * strideA2 ); }
			zlarfb( 'left', 'conjugate-transpose', 'forward', 'columnwise', ihi - i, N - i - IB + 1, IB, A, strideA1, strideA2, offsetA + i * strideA1 + ( i - 1 ) * strideA2, WORK, 1, LDT, offsetWORK + IWT, A, strideA1, strideA2, offsetA + i * strideA1 + ( i + IB - 1 ) * strideA2, WORK, 1, LDWORK, offsetWORK );
		}
	}
	zgehd2( N, i, ihi, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK );
	return 0;
}
module.exports = zgehrd;
