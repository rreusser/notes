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
* Reduces the first NB rows and columns of a real general M-by-N matrix A.
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {NonNegativeInteger} M - TODO
* @param {NonNegativeInteger} N - TODO
* @param {integer} nb - TODO
* @param {Float64Array} A - input matrix
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} d - input array
* @param {integer} strideD - `d` stride length
* @param {Float64Array} e - input array
* @param {integer} strideE - `e` stride length
* @param {Float64Array} TAUQ - input array
* @param {integer} strideTAUQ - `TAUQ` stride length
* @param {Float64Array} TAUP - input array
* @param {integer} strideTAUP - `TAUP` stride length
* @param {Float64Array} X - input matrix
* @param {PositiveInteger} LDX - leading dimension of `X`
* @param {Float64Array} Y - input matrix
* @param {PositiveInteger} LDY - leading dimension of `Y`
* @throws {TypeError} first argument must be a valid order
* @returns {*} result
*/
function dlabrd( order, M, N, nb, A, LDA, d, strideD, e, strideE, TAUQ, strideTAUQ, TAUP, strideTAUP, X, LDX, Y, LDY ) {
	var sa1;
	var sa2;
	var sx1;
	var sx2;
	var sy1;
	var sy2;
	var od;
	var oe;
	var ot;
	var ot;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( order === 'column-major' ) {
		sa1 = 1;
		sa2 = LDA;
		sx1 = 1;
		sx2 = LDX;
		sy1 = 1;
		sy2 = LDY;
	} else {
		sa1 = LDA;
		sa2 = 1;
		sx1 = LDX;
		sx2 = 1;
		sy1 = LDY;
		sy2 = 1;
	}
	od = stride2offset( N, strideD );
	oe = stride2offset( N, strideE );
	ot = stride2offset( N, strideTAUQ );
	ot = stride2offset( N, strideTAUP );
	return base( M, N, nb, A, sa1, sa2, 0, d, strideD, od, e, strideE, oe, TAUQ, strideTAUQ, ot, TAUP, strideTAUP, ot, X, sx1, sx2, 0, Y, sy1, sy2, 0 );
}


// EXPORTS //

module.exports = dlabrd;
