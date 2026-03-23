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

var dnrm2 = require( '../../../../blas/base/dnrm2/lib/base.js' );
var dscal = require( '../../../../blas/base/dscal/lib/base.js' );
var dlamch = require( '../../dlamch/lib/base.js' );
var dlapy2 = require( '../../dlapy2/lib/base.js' );


// MAIN //

/**
* Generates a real elementary reflector H of order N, such that.
*
*   H _ ( alpha ) = ( beta ),   H__T _ H = I.
*       (   x   )   (   0  )
*
* H = I - tau _ ( 1 ) _ ( 1 v**T )
*               ( v )
*
* @private
* @param {NonNegativeInteger} N - order of the reflector
* @param {Float64Array} alpha - scalar, overwritten with beta on exit
* @param {NonNegativeInteger} offsetAlpha - index into alpha array
* @param {Float64Array} x - vector, overwritten with v on exit
* @param {integer} strideX - stride for x
* @param {NonNegativeInteger} offsetX - starting index for x
* @param {Float64Array} tau - output scalar
* @param {NonNegativeInteger} offsetTau - index into tau array
*/
function dlarfg( N, alpha, offsetAlpha, x, strideX, offsetX, tau, offsetTau ) {
	var rsafmn;
	var safmin;
	var xnorm;
	var beta;
	var knt;
	var j;

	if ( N <= 1 ) {
		tau[ offsetTau ] = 0.0;
		return;
	}

	xnorm = dnrm2( N - 1, x, strideX, offsetX );

	if ( xnorm === 0.0 ) {
		// H = I
		tau[ offsetTau ] = 0.0;
	} else {
		// General case
		// Fortran SIGN(A,B): |A| * sign(B)
		beta = -( Math.sign( alpha[ offsetAlpha ] ) || 1.0 ) * dlapy2( alpha[ offsetAlpha ], xnorm );
		safmin = dlamch( 'S' ) / dlamch( 'E' );
		knt = 0;

		if ( Math.abs( beta ) < safmin ) {
			// XNORM, BETA may be inaccurate; scale X and recompute them
			rsafmn = 1.0 / safmin;
			do {
				knt += 1;
				dscal( N - 1, rsafmn, x, strideX, offsetX );
				beta *= rsafmn;
				alpha[ offsetAlpha ] *= rsafmn;
			} while ( Math.abs( beta ) < safmin && knt < 20 );

			// New BETA is at most 1, at least SAFMIN
			xnorm = dnrm2( N - 1, x, strideX, offsetX );
			beta = -( Math.sign( alpha[ offsetAlpha ] ) || 1.0 ) * dlapy2( alpha[ offsetAlpha ], xnorm );
		}

		tau[ offsetTau ] = ( beta - alpha[ offsetAlpha ] ) / beta;
		dscal( N - 1, 1.0 / ( alpha[ offsetAlpha ] - beta ), x, strideX, offsetX );

		// If ALPHA is subnormal, it may lose relative accuracy
		for ( j = 0; j < knt; j++ ) {
			beta *= safmin;
		}
		alpha[ offsetAlpha ] = beta;
	}
}


// EXPORTS //

module.exports = dlarfg;
