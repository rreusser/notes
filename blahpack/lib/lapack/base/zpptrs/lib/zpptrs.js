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
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Solves a system of linear equations `A * X = B` with a complex Hermitian positive definite matrix `A` in packed storage using the Cholesky factorization `A = U**H * U` or `A = L * L**H` computed by zpptrf.
*
* @param {string} order - storage layout (`'row-major'` or `'column-major'`)
* @param {string} uplo - specifies whether the upper or lower triangular factor is stored (`'upper'` or `'lower'`)
* @param {NonNegativeInteger} N - order of matrix `A`
* @param {NonNegativeInteger} nrhs - number of right-hand side columns
* @param {Complex128Array} AP - packed triangular factor (from zpptrf)
* @param {Complex128Array} B - right-hand side matrix, overwritten with solution on exit
* @param {PositiveInteger} LDB - leading dimension of `B`
* @throws {TypeError} first argument must be a valid order
* @returns {integer} info - 0 if successful
*/
function zpptrs( order, uplo, N, nrhs, AP, B, LDB ) {
	var sb1;
	var sb2;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( order === 'column-major' ) {
		sb1 = 1;
		sb2 = LDB;
	} else {
		sb1 = LDB;
		sb2 = 1;
	}
	return base( uplo, N, nrhs, AP, 1, 0, B, sb1, sb2, 0 );
}


// EXPORTS //

module.exports = zpptrs;
