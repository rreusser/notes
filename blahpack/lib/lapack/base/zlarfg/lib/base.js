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

var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var dznrm2 = require( '../../../../blas/base/dznrm2/lib/base.js' );
var zdscal = require( '../../../../blas/base/zdscal/lib/base.js' );
var zscal = require( '../../../../blas/base/zscal/lib/base.js' );
var dlamch = require( '../../dlamch/lib/base.js' );
var dlapy3 = require( '../../dlapy3/lib/base.js' );
var cmplx = require( '../../../../cmplx.js' );

// MAIN //

/**
* Generate a complex elementary reflector H of order N, such that
*
*   H^H * ( alpha ) = ( beta ),   H^H * H = I.
*          (   x   )   (   0  )
*
* where alpha and beta are scalars, with beta real, and x is an
* (N-1)-element complex vector.
*
* H is represented in the form
*
*   H = I - tau * ( 1 ) * ( 1 v^H )
*                 ( v )
*
* @private
* @param {NonNegativeInteger} N - order of the reflector
* @param {Complex128Array} alpha - complex scalar, overwritten with beta
* @param {NonNegativeInteger} offsetAlpha - starting index for alpha (in complex elements)
* @param {Complex128Array} x - complex vector, overwritten with v
* @param {integer} strideX - stride for x (in complex elements)
* @param {NonNegativeInteger} offsetX - starting index for x (in complex elements)
* @param {Complex128Array} tau - output complex scalar
* @param {NonNegativeInteger} offsetTau - starting index for tau (in complex elements)
*/
function zlarfg( N, alpha, offsetAlpha, x, strideX, offsetX, tau, offsetTau ) {
	var rsafmn;
	var safmin;
	var alphr;
	var alphi;
	var xnorm;
	var beta;
	var tauv;
	var tmp;
	var av;
	var oA;
	var oT;
	var knt;
	var j;

	tauv = reinterpret( tau, 0 );
	oT = offsetTau * 2;

	if ( N <= 0 ) {
		tauv[ oT ] = 0.0;
		tauv[ oT + 1 ] = 0.0;
		return;
	}

	av = reinterpret( alpha, 0 );
	oA = offsetAlpha * 2;

	xnorm = dznrm2( N - 1, x, strideX, offsetX );
	alphr = av[ oA ];
	alphi = av[ oA + 1 ];

	if ( xnorm === 0.0 && alphi === 0.0 ) {
		// H = I
		tauv[ oT ] = 0.0;
		tauv[ oT + 1 ] = 0.0;
	} else {
		// General case
		// Fortran SIGN(A,B) returns |A|*sign(B); when B=0, result is +|A|.
		// Math.sign(0) returns 0, so default to 1.0 for alphr=0.
		beta = -( Math.sign( alphr ) || 1.0 ) * dlapy3( alphr, alphi, xnorm );
		safmin = dlamch( 'S' ) / dlamch( 'E' );
		rsafmn = 1.0 / safmin;

		knt = 0;
		if ( Math.abs( beta ) < safmin ) {
			// XNORM, BETA may be inaccurate; scale X and recompute them
			do {
				knt = knt + 1;
				zdscal( N - 1, rsafmn, x, strideX, offsetX );
				beta = beta * rsafmn;
				alphi = alphi * rsafmn;
				alphr = alphr * rsafmn;
			} while ( Math.abs( beta ) < safmin && knt < 20 );

			// New BETA is at most 1, at least SAFMIN
			xnorm = dznrm2( N - 1, x, strideX, offsetX );
			av[ oA ] = alphr;
			av[ oA + 1 ] = alphi;
			// Fortran SIGN(A,B) returns |A|*sign(B); when B=0, result is +|A|.
		// Math.sign(0) returns 0, so default to 1.0 for alphr=0.
		beta = -( Math.sign( alphr ) || 1.0 ) * dlapy3( alphr, alphi, xnorm );
		}
		tauv[ oT ] = ( beta - alphr ) / beta;
		tauv[ oT + 1 ] = -alphi / beta;

		// alpha = 1.0 / (alpha - beta)
		// Use cmplx.divAt for ZLADIV( DCMPLX(ONE), ALPHA - BETA )
		tmp = new Float64Array( 4 );
		tmp[ 0 ] = 1.0;
		tmp[ 1 ] = 0.0;
		tmp[ 2 ] = alphr - beta;
		tmp[ 3 ] = alphi;
		cmplx.divAt( tmp, 0, tmp, 0, tmp, 2 );

		zscal( N - 1, new Complex128( tmp[ 0 ], tmp[ 1 ] ), x, strideX, offsetX );

		// If ALPHA is subnormal, it may lose relative accuracy
		for ( j = 0; j < knt; j++ ) {
			beta = beta * safmin;
		}
		av[ oA ] = beta;
		av[ oA + 1 ] = 0.0;
	}
}


// EXPORTS //

module.exports = zlarfg;
