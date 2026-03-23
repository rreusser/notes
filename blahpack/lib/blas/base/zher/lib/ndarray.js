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

var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Performs the Hermitian rank 1 operation `A = alpha*x*x^H + A` where `A` is an `N` by `N` Hermitian matrix, `x` is an `N` element complex vector, and `alpha` is a real scalar.
*
* @param {string} uplo - specifies whether the upper or lower triangular part of `A` is used
* @param {NonNegativeInteger} N - order of the matrix `A`
* @param {number} alpha - real scalar constant
* @param {Complex128Array} x - input vector
* @param {integer} strideX - `x` stride length
* @param {NonNegativeInteger} offsetX - starting index for `x`
* @param {Complex128Array} A - input/output Hermitian matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @throws {TypeError} first argument must be a valid matrix triangle
* @throws {RangeError} second argument must be a nonnegative integer
* @throws {RangeError} fifth argument must be non-zero
* @returns {Complex128Array} `A`
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
*
* var A = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 1, 0 ] );
* var x = new Complex128Array( [ 1, 0, 0, 1 ] );
*
* zher( 'upper', 2, 1.0, x, 1, 0, A, 1, 2, 0 );
*/
function zher( uplo, N, alpha, x, strideX, offsetX, A, strideA1, strideA2, offsetA ) {
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( strideX === 0 ) {
		throw new RangeError( format( 'invalid argument. Fifth argument must be non-zero. Value: `%d`.', strideX ) );
	}
	if ( N === 0 || alpha === 0.0 ) {
		return A;
	}
	return base( uplo, N, alpha, x, strideX, offsetX, A, strideA1, strideA2, offsetA );
}


// EXPORTS //

module.exports = zher;
