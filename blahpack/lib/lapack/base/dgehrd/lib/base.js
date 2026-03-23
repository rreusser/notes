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

var daxpy = require( '../../../../blas/base/daxpy/lib/base.js' );
var dgehd2 = require( '../../dgehd2/lib/base.js' );
var dgemm = require( '../../../../blas/base/dgemm/lib/base.js' );
var dlahr2 = require( '../../dlahr2/lib/base.js' );
var dlarfb = require( '../../dlarfb/lib/base.js' );
var dtrmm = require( '../../../../blas/base/dtrmm/lib/base.js' );
var NBMAX = 64;
var LDT = NBMAX + 1;
function dgehrd( N, ilo, ihi, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK, lwork ) {
	var LDWORK;
	var IWT;
	var sa1;
	var sa2;
	var oA;
	var oE;
	var NB;
	var NX;
	var NH;
	var IB;
	var ei;
	var i;
	var j;
	NB = 32;
	sa1 = strideA1; sa2 = strideA2; oA = offsetA;
	for ( i = 0; i < ilo - 1; i++ ) { TAU[ offsetTAU + i * strideTAU ] = 0.0; }
	for ( i = Math.max( 0, ihi - 1 ); i < N - 1; i++ ) { TAU[ offsetTAU + i * strideTAU ] = 0.0; }
	NH = ihi - ilo + 1;
	if ( NH <= 1 ) { return 0; }
	NX = Math.max( NB, NB ); LDWORK = N;
	if ( NB < 2 || NB >= NH ) {
		i = ilo;
	} else {
		IWT = N * NB;
		for ( i = ilo; i <= ihi - 1 - NX; i += NB ) {
			IB = Math.min( NB, ihi - i );
			dlahr2( ihi, i, IB, A, strideA1, strideA2, offsetA + ( i - 1 ) * strideA2, TAU, strideTAU, offsetTAU + ( i - 1 ) * strideTAU, WORK, 1, LDT, offsetWORK + IWT, WORK, 1, LDWORK, offsetWORK );
			oE = oA + ( i + IB - 1 ) * sa1 + ( i + IB - 2 ) * sa2;
			ei = A[ oE ]; A[ oE ] = 1.0;
			dgemm( 'no-transpose', 'transpose', ihi, ihi - i - IB + 1, IB, -1.0, WORK, 1, LDWORK, offsetWORK, A, strideA1, strideA2, offsetA + ( i + IB - 1 ) * strideA1 + ( i - 1 ) * strideA2, 1.0, A, strideA1, strideA2, offsetA + ( i + IB - 1 ) * strideA2 );
			A[ oE ] = ei;
			dtrmm( 'right', 'lower', 'transpose', 'unit', i, IB - 1, 1.0, A, strideA1, strideA2, offsetA + i * strideA1 + ( i - 1 ) * strideA2, WORK, 1, LDWORK, offsetWORK );
			for ( j = 0; j < IB - 1; j++ ) { daxpy( i, -1.0, WORK, 1, offsetWORK + LDWORK * j, A, strideA1, offsetA + ( i + j ) * strideA2 ); }
			dlarfb( 'left', 'transpose', 'forward', 'columnwise', ihi - i, N - i - IB + 1, IB, A, strideA1, strideA2, offsetA + i * strideA1 + ( i - 1 ) * strideA2, WORK, 1, LDT, offsetWORK + IWT, A, strideA1, strideA2, offsetA + i * strideA1 + ( i + IB - 1 ) * strideA2, WORK, 1, LDWORK, offsetWORK );
		}
	}
	dgehd2( N, i, ihi, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK );
	return 0;
}
module.exports = dgehrd;
