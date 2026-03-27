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
* Performs one of the matrix-vector operations:.
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {string} trans - `'no-transpose'` or `'transpose'`
* @param {NonNegativeInteger} M - number of rows of A
* @param {NonNegativeInteger} N - number of columns of A
* @param {number} alpha - scalar multiplier for A*x
* @param {Float64Array} A - input matrix
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} x - input array
* @param {integer} strideX - `x` stride length
* @param {number} beta - scalar multiplier for y
* @param {Float64Array} y - input array
* @param {integer} strideY - `y` stride length
* @throws {TypeError} first argument must be a valid order
* @returns {*} result
*/
function dgemv( order, trans, M, N, alpha, A, LDA, x, strideX, beta, y, strideY ) {
	var sa1;
	var sa2;
	var ox;
	var oy;

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
	ox = stride2offset( N, strideX );
	oy = stride2offset( N, strideY );
	return base( trans, M, N, alpha, A, sa1, sa2, 0, x, strideX, ox, beta, y, strideY, oy );
}


// EXPORTS //

module.exports = dgemv;
