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
* Computes an LU factorization of a real M-by-N band matrix A using partial.
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {NonNegativeInteger} M - TODO
* @param {NonNegativeInteger} N - TODO
* @param {NonNegativeInteger} kl - TODO
* @param {NonNegativeInteger} ku - TODO
* @param {Float64Array} AB - input matrix
* @param {PositiveInteger} LDAB - leading dimension of `AB`
* @param {Int32Array} IPIV - input array
* @param {integer} strideIPIV - `IPIV` stride length
* @throws {TypeError} first argument must be a valid order
* @returns {*} result
*/
function dgbtrf( order, M, N, kl, ku, AB, LDAB, IPIV, strideIPIV ) {
	var sa1;
	var sa2;
	var oi;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( order === 'column-major' ) {
		sa1 = 1;
		sa2 = LDAB;
	} else {
		sa1 = LDAB;
		sa2 = 1;
	}
	oi = stride2offset( N, strideIPIV );
	return base( M, N, kl, ku, AB, sa1, sa2, 0, IPIV, strideIPIV, oi );
}


// EXPORTS //

module.exports = dgbtrf;
