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

// MODULES //

var isLayout = require( '@stdlib/blas/base/assert/is-layout' );
var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Compute some or all of the right and/or left eigenvectors of a pair of.
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {*} side - side
* @param {*} howmny - howmny
* @param {*} strideSELECT - strideSELECT
* @param {*} offsetSELECT - offsetSELECT
* @param {*} N - N
* @param {Complex128Array} S - input matrix
* @param {PositiveInteger} LDS - leading dimension of `S`
* @param {Complex128Array} P - input matrix
* @param {PositiveInteger} LDP - leading dimension of `P`
* @param {Complex128Array} VL - input matrix
* @param {PositiveInteger} LDVL - leading dimension of `VL`
* @param {Complex128Array} VR - input matrix
* @param {PositiveInteger} LDVR - leading dimension of `VR`
* @param {*} mm - mm
* @param {*} M - M
* @param {Complex128Array} WORK - input array
* @param {integer} strideWORK - `WORK` stride length
* @param {Float64Array} RWORK - input array
* @param {integer} strideRWORK - `RWORK` stride length
* @throws {TypeError} first argument must be a valid order
* @returns {*} result
*/
function ztgevc( order, side, howmny, SELECT, strideSELECT, offsetSELECT, N, S, LDS, P, LDP, VL, LDVL, VR, LDVR, mm, M, WORK, strideWORK, RWORK, strideRWORK ) {
	var ss1;
	var ss2;
	var sp1;
	var sp2;
	var sv1;
	var sv2;
	var sv1;
	var sv2;
	var ow;
	var or;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( order === 'column-major' ) {
		ss1 = 1;
		ss2 = LDS;
		sp1 = 1;
		sp2 = LDP;
		sv1 = 1;
		sv2 = LDVL;
		sv1 = 1;
		sv2 = LDVR;
	} else {
		ss1 = LDS;
		ss2 = 1;
		sp1 = LDP;
		sp2 = 1;
		sv1 = LDVL;
		sv2 = 1;
		sv1 = LDVR;
		sv2 = 1;
	}
	ow = stride2offset( N, strideWORK );
	or = stride2offset( N, strideRWORK );
	return base( side, howmny, SELECT, strideSELECT, offsetSELECT, N, S, ss1, ss2, 0, P, sp1, sp2, 0, VL, sv1, sv2, 0, VR, sv1, sv2, 0, mm, M, WORK, strideWORK, ow, RWORK, strideRWORK, or );
}


// EXPORTS //

module.exports = ztgevc;
