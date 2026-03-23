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
* Reduces NB rows and columns of a real symmetric matrix A to symmetric.
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {string} uplo - TODO
* @param {NonNegativeInteger} N - TODO
* @param {integer} nb - TODO
* @param {Float64Array} A - input matrix
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} e - input array
* @param {integer} strideE - `e` stride length
* @param {Float64Array} TAU - input array
* @param {integer} strideTAU - `TAU` stride length
* @param {Float64Array} W - input matrix
* @param {PositiveInteger} LDW - leading dimension of `W`
* @throws {TypeError} first argument must be a valid order
* @returns {*} result
*/
function dlatrd( order, uplo, N, nb, A, LDA, e, strideE, TAU, strideTAU, W, LDW ) {
	var sa1;
	var sa2;
	var sw1;
	var sw2;
	var oe;
	var ot;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( order === 'column-major' ) {
		sa1 = 1;
		sa2 = LDA;
		sw1 = 1;
		sw2 = LDW;
	} else {
		sa1 = LDA;
		sa2 = 1;
		sw1 = LDW;
		sw2 = 1;
	}
	oe = stride2offset( N, strideE );
	ot = stride2offset( N, strideTAU );
	return base( uplo, N, nb, A, sa1, sa2, 0, e, strideE, oe, TAU, strideTAU, ot, W, sw1, sw2, 0 );
}


// EXPORTS //

module.exports = dlatrd;
