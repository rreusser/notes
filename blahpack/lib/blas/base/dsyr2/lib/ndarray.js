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
* Performs the symmetric rank 2 operation `A = alpha*x*y^T + alpha*y*x^T + A` where `A` is an `N` by `N` symmetric matrix, `x` and `y` are `N` element vectors, and `alpha` is a scalar.
*
* @param {string} uplo - specifies whether the upper or lower triangular part of `A` is used
* @param {NonNegativeInteger} N - order of the matrix `A`
* @param {number} alpha - scalar constant
* @param {Float64Array} x - first input vector
* @param {integer} strideX - `x` stride length
* @param {NonNegativeInteger} offsetX - starting index for `x`
* @param {Float64Array} y - second input vector
* @param {integer} strideY - `y` stride length
* @param {NonNegativeInteger} offsetY - starting index for `y`
* @param {Float64Array} A - input/output matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @throws {TypeError} first argument must be a valid matrix triangle
* @throws {RangeError} second argument must be a nonnegative integer
* @throws {RangeError} fifth argument must be non-zero
* @throws {RangeError} eighth argument must be non-zero
* @returns {Float64Array} `A`
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
*
* var A = new Float64Array( [ 1, 0, 0, 1 ] );
* var x = new Float64Array( [ 1, 2 ] );
* var y = new Float64Array( [ 3, 4 ] );
*
* dsyr2( 'upper', 2, 1.0, x, 1, 0, y, 1, 0, A, 1, 2, 0 );
*/
function dsyr2( uplo, N, alpha, x, strideX, offsetX, y, strideY, offsetY, A, strideA1, strideA2, offsetA ) {
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( strideX === 0 ) {
		throw new RangeError( format( 'invalid argument. Fifth argument must be non-zero. Value: `%d`.', strideX ) );
	}
	if ( strideY === 0 ) {
		throw new RangeError( format( 'invalid argument. Eighth argument must be non-zero. Value: `%d`.', strideY ) );
	}
	if ( N === 0 || alpha === 0.0 ) {
		return A;
	}
	return base( uplo, N, alpha, x, strideX, offsetX, y, strideY, offsetY, A, strideA1, strideA2, offsetA );
}


// EXPORTS //

module.exports = dsyr2;
