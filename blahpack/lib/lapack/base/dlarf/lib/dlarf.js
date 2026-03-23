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
* Applies a real elementary reflector H to a real M-by-N matrix C,.
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {string} side - TODO
* @param {NonNegativeInteger} M - TODO
* @param {NonNegativeInteger} N - TODO
* @param {Float64Array} v - input array
* @param {integer} strideV - `v` stride length
* @param {number} tau - TODO
* @param {Float64Array} C - input matrix
* @param {PositiveInteger} LDC - leading dimension of `C`
* @param {Float64Array} WORK - input array
* @param {integer} strideWORK - `WORK` stride length
* @throws {TypeError} first argument must be a valid order
* @returns {*} result
*/
function dlarf( order, side, M, N, v, strideV, tau, C, LDC, WORK, strideWORK ) {
	var sc1;
	var sc2;
	var ov;
	var ow;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( order === 'column-major' ) {
		sc1 = 1;
		sc2 = LDC;
	} else {
		sc1 = LDC;
		sc2 = 1;
	}
	ov = stride2offset( N, strideV );
	ow = stride2offset( N, strideWORK );
	return base( side, M, N, v, strideV, ov, tau, C, sc1, sc2, 0, WORK, strideWORK, ow );
}


// EXPORTS //

module.exports = dlarf;
