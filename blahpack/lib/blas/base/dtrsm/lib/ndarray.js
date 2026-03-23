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

var isOperationSide = require( '@stdlib/blas/base/assert/is-operation-side' );
var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var isMatrixTranspose = require( '@stdlib/blas/base/assert/is-transpose-operation' );
var isDiagonalType = require( '@stdlib/blas/base/assert/is-diagonal-type' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Solves one of the matrix equations op(A)_X = alpha_B or X_op(A) = alpha_B, where A is a triangular matrix.
*
* @param {string} side - specifies whether op(A) appears on the left or right of X
* @param {string} uplo - specifies whether A is upper or lower triangular
* @param {string} transa - specifies the form of op(A)
* @param {string} diag - specifies whether A is unit or non-unit triangular
* @param {NonNegativeInteger} M - number of rows of B
* @param {NonNegativeInteger} N - number of columns of B
* @param {number} alpha - scalar constant
* @param {Float64Array} A - input triangular matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} B - input/output matrix
* @param {integer} strideB1 - stride of the first dimension of `B`
* @param {integer} strideB2 - stride of the second dimension of `B`
* @param {NonNegativeInteger} offsetB - starting index for `B`
* @throws {TypeError} first argument must be a valid operation side
* @throws {TypeError} second argument must be a valid matrix triangle
* @throws {TypeError} third argument must be a valid transpose operation
* @throws {TypeError} fourth argument must be a valid diagonal type
* @throws {RangeError} fifth argument must be a nonnegative integer
* @throws {RangeError} sixth argument must be a nonnegative integer
* @returns {Float64Array} `B`
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
*
* var A = new Float64Array( [ 2.0, 0.0, 3.0, 4.0 ] );
* var B = new Float64Array( [ 8.0, 4.0, 10.0, 12.0 ] );
*
* dtrsm( 'left', 'upper', 'no-transpose', 'non-unit', 2, 2, 1.0, A, 1, 2, 0, B, 1, 2, 0 );
* // B => <Float64Array>[ 2.5, 1, 0.5, 3 ]
*/
function dtrsm( side, uplo, transa, diag, M, N, alpha, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB ) {
	if ( !isOperationSide( side ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid operation side. Value: `%s`.', side ) );
	}
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( !isMatrixTranspose( transa ) ) {
		throw new TypeError( format( 'invalid argument. Third argument must be a valid transpose operation. Value: `%s`.', transa ) );
	}
	if ( !isDiagonalType( diag ) ) {
		throw new TypeError( format( 'invalid argument. Fourth argument must be a valid diagonal type. Value: `%s`.', diag ) );
	}
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. Fifth argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Sixth argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( M === 0 || N === 0 ) {
		return B;
	}
	return base( side, uplo, transa, diag, M, N, alpha, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB );
}


// EXPORTS //

module.exports = dtrsm;
