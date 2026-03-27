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
* Solve a complex banded system using LU factorization.
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {string} trans - 'no-transpose' for A*X=B, 'transpose' for A^T*X=B, 'conjugate-transpose' for A^H*X=B
* @param {NonNegativeInteger} N - order of matrix A
* @param {NonNegativeInteger} kl - number of subdiagonals
* @param {NonNegativeInteger} ku - number of superdiagonals
* @param {NonNegativeInteger} nrhs - number of right-hand sides
* @param {Float64Array} AB - input matrix
* @param {PositiveInteger} LDAB - leading dimension of `AB`
* @param {Int32Array} IPIV - input array
* @param {integer} strideIPIV - `IPIV` stride length
* @param {Float64Array} B - input matrix
* @param {PositiveInteger} LDB - leading dimension of `B`
* @throws {TypeError} first argument must be a valid order
* @returns {*} result
*/
function zgbtrs( order, trans, N, kl, ku, nrhs, AB, LDAB, IPIV, strideIPIV, B, LDB ) {
	var sa1;
	var sa2;
	var sb1;
	var sb2;
	var oi;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( order === 'column-major' ) {
		sa1 = 1;
		sa2 = LDAB;
		sb1 = 1;
		sb2 = LDB;
	} else {
		sa1 = LDAB;
		sa2 = 1;
		sb1 = LDB;
		sb2 = 1;
	}
	oi = stride2offset( N, strideIPIV );
	return base( trans, N, kl, ku, nrhs, AB, sa1, sa2, 0, IPIV, strideIPIV, oi, B, sb1, sb2, 0 );
}


// EXPORTS //

module.exports = zgbtrs;
