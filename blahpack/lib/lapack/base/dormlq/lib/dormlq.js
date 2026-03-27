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
* Overwrites the M-by-N matrix C with Q.
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {string} side - `'left'` to apply Q from left, `'right'` from right
* @param {string} trans - `'no-transpose'` for Q, `'transpose'` for Q^T
* @param {NonNegativeInteger} M - number of rows of C
* @param {NonNegativeInteger} N - number of columns of C
* @param {NonNegativeInteger} K - number of elementary reflectors
* @param {Float64Array} A - input matrix
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} TAU - input array
* @param {integer} strideTAU - `TAU` stride length
* @param {Float64Array} C - input matrix
* @param {PositiveInteger} LDC - leading dimension of `C`
* @param {Float64Array} WORK - input array
* @param {integer} strideWORK - `WORK` stride length
* @throws {TypeError} first argument must be a valid order
* @returns {*} result
*/
function dormlq( order, side, trans, M, N, K, A, LDA, TAU, strideTAU, C, LDC, WORK, strideWORK ) {
	var sa1;
	var sa2;
	var sc1;
	var sc2;
	var ot;
	var ow;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( order === 'column-major' ) {
		sa1 = 1;
		sa2 = LDA;
		sc1 = 1;
		sc2 = LDC;
	} else {
		sa1 = LDA;
		sa2 = 1;
		sc1 = LDC;
		sc2 = 1;
	}
	ot = stride2offset( N, strideTAU );
	ow = stride2offset( N, strideWORK );
	return base(side, trans, M, N, K, A, sa1, sa2, 0, TAU, strideTAU, ot, C, sc1, sc2, 0, WORK, strideWORK, ow );
}


// EXPORTS //

module.exports = dormlq;
