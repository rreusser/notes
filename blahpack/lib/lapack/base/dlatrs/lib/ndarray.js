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

var isDiagonalType = require( '@stdlib/blas/base/assert/is-diagonal-type' );
var isMatrixTranspose = require( '@stdlib/blas/base/assert/is-transpose-operation' );
var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Solves a triangular system with scaling to prevent overflow.
*
* @param {string} uplo - `'upper'` or `'lower'`
* @param {string} trans - `'no-transpose'` or `'transpose'`
* @param {string} diag - `'unit'` or `'non-unit'`
* @param {string} normin - `'yes'` if CNORM contains column norms on input, `'no'` to compute them
* @param {NonNegativeInteger} N - order of the matrix
* @param {Float64Array} A - N-by-N triangular matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} x - in/out right-hand side vector of length N
* @param {integer} strideX - stride length for `x`
* @param {NonNegativeInteger} offsetX - starting index for `x`
* @param {Float64Array} scale - out: scale[0] is the scale factor s
* @param {Float64Array} CNORM - in/out column norm array of length N
* @param {integer} strideCNORM - stride length for `CNORM`
* @param {NonNegativeInteger} offsetCNORM - starting index for `CNORM`
* @throws {TypeError} first argument must be a valid matrix triangle
* @throws {TypeError} second argument must be a valid transpose operation
* @throws {TypeError} third argument must be a valid diagonal type
* @throws {TypeError} fourth argument must be a valid norm-in flag
* @throws {RangeError} fifth argument must be a nonnegative integer
* @returns {integer} info - 0 if successful
*/
function dlatrs( uplo, trans, diag, normin, N, A, strideA1, strideA2, offsetA, x, strideX, offsetX, scale, CNORM, strideCNORM, offsetCNORM ) {
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( !isMatrixTranspose( trans ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid transpose operation. Value: `%s`.', trans ) );
	}
	if ( !isDiagonalType( diag ) ) {
		throw new TypeError( format( 'invalid argument. Third argument must be a valid diagonal type. Value: `%s`.', diag ) );
	}
	if ( normin !== 'yes' && normin !== 'no' ) {
		throw new TypeError( format( 'invalid argument. Fourth argument must be a valid norm-in flag. Value: `%s`.', normin ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Fifth argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( N === 0 ) {
		return 0;
	}
	return base( uplo, trans, diag, normin, N, A, strideA1, strideA2, offsetA, x, strideX, offsetX, scale, CNORM, strideCNORM, offsetCNORM );
}


// EXPORTS //

module.exports = dlatrs;
