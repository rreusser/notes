/**
* @license Apache-2.0
*
* Copyright (c) 2025 Ricky Reusser.
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

// VARIABLES //

// LA_CONSTANTS for double precision:
// dsafmin = 2^(-1022)
// dsafmax = 1/dsafmin = 2^1022
var SAFMIN = 2.2250738585072014e-308;
var SAFMAX = 4.49423283715579e+307;

// MAIN //

/**
* Generates a plane rotation so that:
*
*    [  c  s ] . [ f ] = [ r ]
*    [ -s  c ]   [ g ]   [ 0 ]
*
* where c^2 + s^2 = 1.
*
* ## Notes
*
* -   The mathematical formulas used are:
*     -   r = sign(f) * sqrt(f^2 + g^2)
*     -   c = f / r
*     -   s = g / r
*     Hence c >= 0.
* -   The algorithm incorporates scaling to avoid overflow or underflow.
* -   If g = 0, then c = 1 and s = 0.
* -   If f = 0 and g != 0, then c = 0 and s = sign(1, g).
*
* @private
* @param {number} f - first component of the vector to be rotated
* @param {number} g - second component of the vector to be rotated
* @returns {Object} object with properties `c` (cosine), `s` (sine), and `r` (radius)
*/
function dlartg( f, g ) {
	var rtmin;
	var rtmax;
	var f1;
	var g1;
	var fs;
	var gs;
	var d;
	var c;
	var s;
	var r;
	var u;

	rtmin = Math.sqrt( SAFMIN );
	rtmax = Math.sqrt( SAFMAX / 2.0 );

	f1 = Math.abs( f );
	g1 = Math.abs( g );

	if ( g === 0.0 ) {
		// g is zero: rotation is identity
		c = 1.0;
		s = 0.0;
		r = f;
	} else if ( f === 0.0 ) {
		// f is zero
		c = 0.0;
		s = ( g > 0.0 ) ? 1.0 : -1.0; // sign(1, g)
		r = g1;
	} else if ( f1 > rtmin && f1 < rtmax && g1 > rtmin && g1 < rtmax ) {
		// Both f and g are in a safe range: unscaled algorithm
		d = Math.sqrt( f * f + g * g );
		c = f1 / d;
		r = ( f > 0.0 ) ? d : -d; // sign(d, f)
		s = g / r;
	} else {
		// Scaled algorithm to avoid overflow/underflow
		u = Math.min( SAFMAX, Math.max( SAFMIN, f1, g1 ) );
		fs = f / u;
		gs = g / u;
		d = Math.sqrt( fs * fs + gs * gs );
		c = Math.abs( fs ) / d;
		r = ( f > 0.0 ) ? d : -d; // sign(d, f)
		s = gs / r;
		r = r * u;
	}

	return {
		'c': c,
		's': s,
		'r': r
	};
}


// EXPORTS //

module.exports = dlartg;
