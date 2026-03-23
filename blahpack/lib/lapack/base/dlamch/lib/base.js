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


// VARIABLES //

// IEEE 754 double-precision constants:
var EPS = 1.1102230246251565e-16;          // 2^-53 (half of Number.EPSILON)
var SFMIN = 2.2250738585072014e-308;       // Number.MIN_VALUE (normalized)
var BASE = 2;                              // radix
var PREC = EPS * BASE;                     // eps * radix
var DIGITS = 53;                           // significand bits
var RND = 1.0;                             // 1.0 for IEEE rounding
var EMIN = -1021;                          // min exponent
var RMIN = 2.2250738585072014e-308;        // tiny = 2^emin (same as sfmin for IEEE)
var EMAX = 1024;                           // max exponent
var RMAX = 1.7976931348623157e+308;        // Number.MAX_VALUE


// MAIN //

/**
* Determines double-precision machine parameters.
*
* @private
* @param {string} cmach - specifies the machine parameter
* @returns {number} machine parameter value
*/
function dlamch( cmach ) {
	var c = cmach.charAt( 0 ).toUpperCase();
	if ( c === 'E' ) {
		return EPS;
	}
	if ( c === 'S' ) {
		return SFMIN;
	}
	if ( c === 'B' ) {
		return BASE;
	}
	if ( c === 'P' ) {
		return PREC;
	}
	if ( c === 'N' ) {
		return DIGITS;
	}
	if ( c === 'R' ) {
		return RND;
	}
	if ( c === 'M' ) {
		return EMIN;
	}
	if ( c === 'U' ) {
		return RMIN;
	}
	if ( c === 'L' ) {
		return EMAX;
	}
	if ( c === 'O' ) {
		return RMAX;
	}
	return 0.0;
}


// EXPORTS //

module.exports = dlamch;
