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
var max = require( '@stdlib/math/base/special/fast/max' );
var base = require( './base.js' );


// MAIN //

/**
* Reduce NB columns of a general matrix in Hessenberg form.
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {NonNegativeInteger} N - order of the matrix A
* @param {NonNegativeInteger} K - offset for the reduction
* @param {NonNegativeInteger} nb - number of columns to reduce
* @param {Float64Array} A - input matrix
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} tau - input array
* @param {integer} strideTAU - `tau` stride length
* @param {Float64Array} t - input array
* @param {integer} strideT - `t` stride length
* @param {*} ldt - ldt
* @param {Float64Array} y - input array
* @param {integer} strideY - `y` stride length
* @param {*} ldy - ldy
* @throws {TypeError} first argument must be a valid order
* @returns {*} result
*/
function dlahr2( order, N, K, nb, A, LDA, tau, strideTAU, t, strideT, ldt, y, strideY, ldy ) {
	var sa1;
	var sa2;
	var ot;
	var ot;
	var oy;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( K < 0 ) {
		throw new RangeError( format( 'invalid argument. Third argument must be a nonnegative integer. Value: `%d`.', K ) );
	}
	if ( LDA < max( 1, N ) ) {
		throw new RangeError( format( 'invalid argument. Sixth argument must be greater than or equal to max(1,N). Value: `%d`.', LDA ) );
	}
	if ( order === 'column-major' ) {
		sa1 = 1;
		sa2 = LDA;
	} else {
		sa1 = LDA;
		sa2 = 1;
	}
	ot = stride2offset( N, strideTAU );
	ot = stride2offset( N, strideT );
	oy = stride2offset( N, strideY );
	return base( N, K, nb, A, sa1, sa2, 0, tau, strideTAU, ot, t, strideT, ot, ldt, y, strideY, oy, ldy );
}


// EXPORTS //

module.exports = dlahr2;
