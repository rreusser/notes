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

var isMatrixTranspose = require( '@stdlib/blas/base/assert/is-transpose-operation' );
var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var isDiagonalType = require( '@stdlib/blas/base/assert/is-diagonal-type' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Solves one of the systems of equations `A*x = b`, `A**T*x = b`, or `A**H*x = b` where `A` is an `N` by `N` upper or lower triangular band complex matrix with `K+1` diagonals.
*
* @param {string} uplo - specifies whether `A` is upper or lower triangular
* @param {string} trans - specifies the operation to be performed
* @param {string} diag - specifies whether `A` is unit or non-unit triangular
* @param {NonNegativeInteger} N - order of the matrix `A`
* @param {NonNegativeInteger} K - number of super/sub-diagonals
* @param {Complex128Array} A - band matrix in band storage
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Complex128Array} x - input/output vector
* @param {integer} strideX - `x` stride length
* @param {NonNegativeInteger} offsetX - starting index for `x`
* @throws {TypeError} first argument must be a valid matrix triangle
* @throws {TypeError} second argument must be a valid transpose operation
* @throws {TypeError} third argument must be a valid diagonal type
* @throws {RangeError} fourth argument must be a nonnegative integer
* @throws {RangeError} fifth argument must be a nonnegative integer
* @throws {RangeError} eleventh argument must be non-zero
* @returns {Complex128Array} `x`
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
*
* var AB = new Complex128Array( 8 );
* var x = new Complex128Array( [ 1, 0, 1, 1 ] );
*
* ztbsv( 'upper', 'no-transpose', 'non-unit', 2, 1, AB, 1, 2, 0, x, 1, 0 );
*/
function ztbsv( uplo, trans, diag, N, K, A, strideA1, strideA2, offsetA, x, strideX, offsetX ) {
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( !isMatrixTranspose( trans ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid transpose operation. Value: `%s`.', trans ) );
	}
	if ( !isDiagonalType( diag ) ) {
		throw new TypeError( format( 'invalid argument. Third argument must be a valid diagonal type. Value: `%s`.', diag ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( K < 0 ) {
		throw new RangeError( format( 'invalid argument. Fifth argument must be a nonnegative integer. Value: `%d`.', K ) );
	}
	if ( strideX === 0 ) {
		throw new RangeError( format( 'invalid argument. Eleventh argument must be non-zero. Value: `%d`.', strideX ) );
	}
	if ( N === 0 ) {
		return x;
	}
	return base( uplo, trans, diag, N, K, A, strideA1, strideA2, offsetA, x, strideX, offsetX );
}


// EXPORTS //

module.exports = ztbsv;
