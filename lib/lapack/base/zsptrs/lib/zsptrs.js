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
var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var format = require( '@stdlib/string/format' );
var max = require( '@stdlib/math/base/special/fast/max' );
var base = require( './base.js' );


// MAIN //

/**
* Solves a system of linear equations `A * X = B` with a complex symmetric.
* matrix A stored in packed format, using the factorization `A = U * D * U**T`
* or `A = L * D * L**T` computed by zsptrf.
*
* @param {string} order - storage layout (`'row-major'` or `'column-major'`)
* @param {string} uplo - specifies the triangle (`'upper'` or `'lower'`)
* @param {NonNegativeInteger} N - order of the matrix
* @param {NonNegativeInteger} nrhs - number of right-hand sides
* @param {Complex128Array} AP - factored packed symmetric matrix
* @param {Int32Array} IPIV - pivot indices from zsptrf
* @param {Complex128Array} B - right-hand side matrix, overwritten with solution
* @param {PositiveInteger} LDB - leading dimension of B
* @throws {TypeError} first argument must be a valid order
* @throws {TypeError} second argument must be a valid matrix triangle
* @returns {integer} status code (0 = success)
*/
function zsptrs( order, uplo, N, nrhs, AP, IPIV, B, LDB ) {
	var sb1;
	var sb2;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Third argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( nrhs < 0 ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be a nonnegative integer. Value: `%d`.', nrhs ) );
	}
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( LDB < max( 1, N ) ) {
		throw new RangeError( format( 'invalid argument. Eighth argument must be greater than or equal to max(1,N). Value: `%d`.', LDB ) );
	}
	if ( order === 'column-major' ) {
		sb1 = 1;
		sb2 = LDB;
	} else {
		sb1 = LDB;
		sb2 = 1;
	}
	return base( uplo, N, nrhs, AP, 1, 0, IPIV, 1, 0, B, sb1, sb2, 0 );
}


// EXPORTS //

module.exports = zsptrs;
