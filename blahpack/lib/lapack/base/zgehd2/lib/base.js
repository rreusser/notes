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

var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlarf = require( '../../zlarf/lib/base.js' );
var zlarfg = require( '../../zlarfg/lib/base.js' );
var SCRATCH_TAU = new Complex128Array( 1 );
var SCRATCH_TAUv = reinterpret( SCRATCH_TAU, 0 );
var SCRATCH_ALPHA = new Complex128Array( 1 );
var SCRATCH_ALPHAv = reinterpret( SCRATCH_ALPHA, 0 );
function zgehd2( N, ilo, ihi, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK ) {
	var xStart;
	var oAlpha;
	var tauv;
	var oTau;
	var sa1;
	var sa2;
	var av;
	var oA;
	var i;
	av = reinterpret( A, 0 );
	sa1 = strideA1 * 2;
	sa2 = strideA2 * 2;
	oA = offsetA * 2;
	tauv = reinterpret( TAU, 0 );
	for ( i = ilo - 1; i < ihi - 1; i++ ) {
		oAlpha = oA + (( i + 1 ) * sa1) + (i * sa2);
		SCRATCH_ALPHAv[ 0 ] = av[ oAlpha ];
		SCRATCH_ALPHAv[ 1 ] = av[ oAlpha + 1 ];
		xStart = Math.min( i + 2, N - 1 );
		zlarfg( ihi - i - 1, SCRATCH_ALPHA, 0, A, strideA1, offsetA + (xStart * strideA1) + (i * strideA2), SCRATCH_TAU, 0 );
		av[ oAlpha ] = 1.0;
		av[ oAlpha + 1 ] = 0.0;
		zlarf( 'right', ihi, ihi - i - 1, A, strideA1, offsetA + (( i + 1 ) * strideA1) + (i * strideA2), SCRATCH_TAU, 0, A, strideA1, strideA2, offsetA + (( i + 1 ) * strideA2), WORK, strideWORK, offsetWORK );
		SCRATCH_TAUv[ 1 ] = -SCRATCH_TAUv[ 1 ];
		zlarf( 'left', ihi - i - 1, N - i - 1, A, strideA1, offsetA + (( i + 1 ) * strideA1) + (i * strideA2), SCRATCH_TAU, 0, A, strideA1, strideA2, offsetA + (( i + 1 ) * strideA1) + (( i + 1 ) * strideA2), WORK, strideWORK, offsetWORK );
		av[ oAlpha ] = SCRATCH_ALPHAv[ 0 ];
		av[ oAlpha + 1 ] = SCRATCH_ALPHAv[ 1 ];
		oTau = ( offsetTAU + (i * strideTAU) ) * 2;
		tauv[ oTau ] = SCRATCH_TAUv[ 0 ];
		tauv[ oTau + 1 ] = -SCRATCH_TAUv[ 1 ];
	}
	return 0;
}
module.exports = zgehd2;
