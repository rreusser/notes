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
* Reduces a complex Hermitian matrix A to real symmetric tridiagonal form T.
*
* @param {string} uplo - `'upper'` or `'lower'`
* @param {NonNegativeInteger} N - order of the matrix A
* @param {Complex128Array} A - input/output Hermitian matrix
* @param {integer} strideA1 - stride of the first dimension of A (complex elements)
* @param {integer} strideA2 - stride of the second dimension of A (complex elements)
* @param {NonNegativeInteger} offsetA - starting index for A (complex elements)
* @param {Float64Array} d - output diagonal elements of T (length N)
* @param {integer} strideD - stride for d
* @param {NonNegativeInteger} offsetD - starting index for d
* @param {Float64Array} e - output off-diagonal elements of T (length N-1)
* @param {integer} strideE - stride for e
* @param {NonNegativeInteger} offsetE - starting index for e
* @param {Complex128Array} TAU - output scalar factors of reflectors (length N-1)
* @param {integer} strideTAU - stride for TAU (complex elements)
* @param {NonNegativeInteger} offsetTAU - starting index for TAU (complex elements)
* @throws {TypeError} first argument must be a valid matrix triangle
* @throws {RangeError} second argument must be a nonnegative integer
* @returns {integer} info (0 = success)
*/
function zhetd2( uplo, N, A, strideA1, strideA2, offsetA, d, strideD, offsetD, e, strideE, offsetE, TAU, strideTAU, offsetTAU ) {
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( N === 0 ) {
		return 0;
	}
	return base( uplo, N, A, strideA1, strideA2, offsetA, d, strideD, offsetD, e, strideE, offsetE, TAU, strideTAU, offsetTAU );
}


// EXPORTS //

module.exports = zhetd2;
