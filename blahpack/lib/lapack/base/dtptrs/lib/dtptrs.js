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
* Solves a triangular system of the form `A*X = B`, `A^T*X = B`, or `A^H*X = B` where A is stored in packed format.
*
* @param {string} order - storage layout (`'row-major'` or `'column-major'`)
* @param {string} uplo - specifies whether the matrix is upper or lower triangular (`'upper'` or `'lower'`)
* @param {string} trans - specifies the operation (`'no-transpose'` or `'transpose'`)
* @param {string} diag - specifies whether the matrix is unit triangular (`'unit'` or `'non-unit'`)
* @param {NonNegativeInteger} N - order of the matrix A
* @param {NonNegativeInteger} nrhs - number of right-hand side columns
* @param {Float64Array} AP - packed triangular matrix A
* @param {Float64Array} B - right-hand side matrix
* @param {PositiveInteger} LDB - leading dimension of `B`
* @throws {TypeError} first argument must be a valid order
* @returns {integer} status code
*/
function dtptrs( order, uplo, trans, diag, N, nrhs, AP, B, LDB ) {
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
	return base( uplo, trans, diag, N, nrhs, AP, 1, 0, B, sb1, sb2, 0 );
}


// EXPORTS //

module.exports = dtptrs;
