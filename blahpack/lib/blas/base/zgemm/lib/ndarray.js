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
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Performs one of the complex matrix-matrix operations `C = alpha*op(A)*op(B) + beta*C` where op(X) is one of `op(X) = X`, `op(X) = X^T`, or `op(X) = X^H`.
*
* @param {string} transa - specifies the operation for matrix `A`
* @param {string} transb - specifies the operation for matrix `B`
* @param {NonNegativeInteger} M - number of rows of `C`
* @param {NonNegativeInteger} N - number of columns of `C`
* @param {NonNegativeInteger} K - inner dimension
* @param {Complex128} alpha - scalar constant
* @param {Complex128Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Complex128Array} B - input matrix
* @param {integer} strideB1 - stride of the first dimension of `B`
* @param {integer} strideB2 - stride of the second dimension of `B`
* @param {NonNegativeInteger} offsetB - starting index for `B`
* @param {Complex128} beta - scalar constant
* @param {Complex128Array} C - output matrix
* @param {integer} strideC1 - stride of the first dimension of `C`
* @param {integer} strideC2 - stride of the second dimension of `C`
* @param {NonNegativeInteger} offsetC - starting index for `C`
* @throws {TypeError} first argument must be a valid transpose operation
* @throws {TypeError} second argument must be a valid transpose operation
* @throws {RangeError} third argument must be a nonnegative integer
* @throws {RangeError} fourth argument must be a nonnegative integer
* @throws {RangeError} fifth argument must be a nonnegative integer
* @returns {Complex128Array} `C`
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Complex128 = require( '@stdlib/complex/float64/ctor' );
*
* var A = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 1, 0 ] );
* var B = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 1, 0 ] );
* var C = new Complex128Array( 4 );
* var alpha = new Complex128( 1, 0 );
* var beta = new Complex128( 0, 0 );
*
* zgemm( 'no-transpose', 'no-transpose', 2, 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
*/
function zgemm( transa, transb, M, N, K, alpha, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, beta, C, strideC1, strideC2, offsetC ) {
	if ( !isMatrixTranspose( transa ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid transpose operation. Value: `%s`.', transa ) );
	}
	if ( !isMatrixTranspose( transb ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid transpose operation. Value: `%s`.', transb ) );
	}
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. Third argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( K < 0 ) {
		throw new RangeError( format( 'invalid argument. Fifth argument must be a nonnegative integer. Value: `%d`.', K ) );
	}
	if ( M === 0 || N === 0 ) {
		return C;
	}
	return base( transa, transb, M, N, K, alpha, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, beta, C, strideC1, strideC2, offsetC );
}


// EXPORTS //

module.exports = zgemm;
