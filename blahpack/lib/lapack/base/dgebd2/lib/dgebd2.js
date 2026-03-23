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
* Reduces a real M-by-N matrix A to upper or lower bidiagonal form B.
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {NonNegativeInteger} M - TODO
* @param {NonNegativeInteger} N - TODO
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
* @param {Float64Array} WORK - input array
* @param {integer} strideWORK - `WORK` stride length
* @throws {TypeError} first argument must be a valid order
* @returns {*} result
*/
function dgebd2( order, M, N, A, LDA, d, strideD, e, strideE, TAUQ, strideTAUQ, TAUP, strideTAUP, WORK, strideWORK ) {
	var sa1;
	var sa2;
	var od;
	var oe;
	var ot;
	var ot;
	var ow;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( order === 'column-major' ) {
		sa1 = 1;
		sa2 = LDA;
	} else {
		sa1 = LDA;
		sa2 = 1;
	}
	od = stride2offset( N, strideD );
	oe = stride2offset( N, strideE );
	ot = stride2offset( N, strideTAUQ );
	ot = stride2offset( N, strideTAUP );
	ow = stride2offset( N, strideWORK );
	return base( M, N, A, sa1, sa2, 0, d, strideD, od, e, strideE, oe, TAUQ, strideTAUQ, ot, TAUP, strideTAUP, ot, WORK, strideWORK, ow );
}


// EXPORTS //

module.exports = dgebd2;
