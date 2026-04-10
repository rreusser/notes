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

var dnrm2 = require( './../../../../blas/base/dnrm2/lib/base.js' );
var dscal = require( './../../../../blas/base/dscal/lib/base.js' );
var dlamch = require( './../../dlamch/lib/base.js' );
var dlapy2 = require( './../../dlapy2/lib/base.js' );


// MAIN //

/**
* Generates a real elementary reflector H of order N with non-negative beta.
*
* `H*( alpha ) = ( beta )`,   `H**T*H = I`, where `beta >= 0`.
* `(   x   )   (   0  )`
*
* Variant of `dlarfg` used by `dgeqr2p`/`dgeqrfp`.
*
* @private
* @param {NonNegativeInteger} N - order of the reflector
* @param {Float64Array} alpha - scalar, overwritten with beta on exit
* @param {NonNegativeInteger} offsetAlpha - index into `alpha`
* @param {Float64Array} x - vector of length `1+(N-2)*abs(strideX)`, overwritten with `v` on exit
* @param {integer} strideX - stride for `x`
* @param {NonNegativeInteger} offsetX - starting index for `x`
* @param {Float64Array} tau - output scalar
* @param {NonNegativeInteger} offsetTau - index into `tau`
*/
function dlarfgp( N, alpha, offsetAlpha, x, strideX, offsetX, tau, offsetTau ) {
	var savealpha;
	var bignum;
	var smlnum;
	var xnorm;
	var beta;
	var sign;
	var eps;
	var knt;
	var j;

	if ( N <= 0 ) {
		tau[ offsetTau ] = 0.0;
		return;
	}

	eps = dlamch( 'precision' );
	xnorm = dnrm2( N - 1, x, strideX, offsetX );

	if ( xnorm <= eps * Math.abs( alpha[ offsetAlpha ] ) ) {
		// H = I
		if ( alpha[ offsetAlpha ] >= 0.0 ) {
			tau[ offsetTau ] = 0.0;
		} else {
			tau[ offsetTau ] = 2.0;
			for ( j = 0; j < N - 1; j++ ) {
				x[ offsetX + ( j * strideX ) ] = 0.0;
			}
			alpha[ offsetAlpha ] = -alpha[ offsetAlpha ];
		}
		return;
	}

	// General case; Fortran SIGN(A,B): |A| * sign(B), with sign(0)=+1.
	sign = ( alpha[ offsetAlpha ] >= 0.0 ) ? 1.0 : -1.0;
	beta = sign * dlapy2( alpha[ offsetAlpha ], xnorm );
	smlnum = dlamch( 'safe-minimum' ) / dlamch( 'epsilon' );
	knt = 0;
	if ( Math.abs( beta ) < smlnum ) {
		// XNORM, BETA may be inaccurate; scale X and recompute:
		bignum = 1.0 / smlnum;
		do {
			knt += 1;
			dscal( N - 1, bignum, x, strideX, offsetX );
			beta *= bignum;
			alpha[ offsetAlpha ] *= bignum;
		} while ( Math.abs( beta ) < smlnum && knt < 20 );

		// New BETA is at most 1, at least SMLNUM:
		xnorm = dnrm2( N - 1, x, strideX, offsetX );
		sign = ( alpha[ offsetAlpha ] >= 0.0 ) ? 1.0 : -1.0;
		beta = sign * dlapy2( alpha[ offsetAlpha ], xnorm );
	}
	savealpha = alpha[ offsetAlpha ];
	alpha[ offsetAlpha ] += beta;
	if ( beta < 0.0 ) {
		beta = -beta;
		tau[ offsetTau ] = -alpha[ offsetAlpha ] / beta;
	} else {
		alpha[ offsetAlpha ] = xnorm * ( xnorm / alpha[ offsetAlpha ] );
		tau[ offsetTau ] = alpha[ offsetAlpha ] / beta;
		alpha[ offsetAlpha ] = -alpha[ offsetAlpha ];
	}

	if ( Math.abs( tau[ offsetTau ] ) <= smlnum ) {
		// In the case where the computed TAU ends up being a denormalized number, it loses relative accuracy. This is a BIG problem. Solution: flush TAU to ZERO. This explains the next IF statement.
		if ( savealpha >= 0.0 ) {
			tau[ offsetTau ] = 0.0;
		} else {
			tau[ offsetTau ] = 2.0;
			for ( j = 0; j < N - 1; j++ ) {
				x[ offsetX + ( j * strideX ) ] = 0.0;
			}
			beta = -savealpha;
		}
	} else {
		// This is the general case.
		dscal( N - 1, 1.0 / alpha[ offsetAlpha ], x, strideX, offsetX );
	}

	// If BETA is subnormal, it may lose relative accuracy:
	for ( j = 0; j < knt; j++ ) {
		beta *= smlnum;
	}
	alpha[ offsetAlpha ] = beta;
}


// EXPORTS //

module.exports = dlarfgp;
