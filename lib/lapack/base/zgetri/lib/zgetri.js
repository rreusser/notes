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
* Computes the inverse of a matrix using the LU factorization computed by zgetrf.
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {NonNegativeInteger} N - order of the matrix A
* @param {Complex128Array} A - input matrix
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Int32Array} IPIV - input array
* @param {integer} strideIPIV - `IPIV` stride length
* @param {Complex128Array} WORK - input array
* @param {integer} strideWORK - `WORK` stride length
* @param {integer} lwork - length of the WORK array (complex elements); should be at least N for unblocked, N*NB for blocked
* @throws {TypeError} first argument must be a valid order
* @returns {*} result
*/
function zgetri( order, N, A, LDA, IPIV, strideIPIV, WORK, strideWORK, lwork ) {
	var sa1;
	var sa2;
	var oi;
	var ow;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( LDA < max( 1, N ) ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be greater than or equal to max(1,N). Value: `%d`.', LDA ) );
	}
	if ( order === 'column-major' ) {
		sa1 = 1;
		sa2 = LDA;
	} else {
		sa1 = LDA;
		sa2 = 1;
	}
	oi = stride2offset( N, strideIPIV );
	ow = stride2offset( N, strideWORK );
	return base( N, A, sa1, sa2, 0, IPIV, strideIPIV, oi, WORK, strideWORK, ow, lwork );
}


// EXPORTS //

module.exports = zgetri;
