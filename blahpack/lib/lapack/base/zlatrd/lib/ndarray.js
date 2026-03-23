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
* Reduces NB rows and columns of a complex Hermitian matrix A to Hermitian.
*
* @param {string} uplo - 'U' for upper, 'L' for lower
* @param {NonNegativeInteger} N - order of matrix A
* @param {integer} nb - number of rows/columns to reduce
* @param {Complex128Array} A - complex Hermitian matrix (complex-element strides)
* @param {integer} strideA1 - stride of first dimension of A (complex elements)
* @param {integer} strideA2 - stride of second dimension of A (complex elements)
* @param {NonNegativeInteger} offsetA - starting index for A (complex elements)
* @param {Float64Array} e - off-diagonal elements
* @param {integer} strideE - stride for e
* @param {NonNegativeInteger} offsetE - starting index for e
* @param {Complex128Array} TAU - scalar factors of reflectors
* @param {integer} strideTAU - stride for TAU (complex elements)
* @param {NonNegativeInteger} offsetTAU - starting index for TAU (complex elements)
* @param {Complex128Array} W - output workspace matrix (N-by-NB) (complex-element strides)
* @param {integer} strideW1 - stride of first dimension of W (complex elements)
* @param {integer} strideW2 - stride of second dimension of W (complex elements)
* @param {NonNegativeInteger} offsetW - starting index for W (complex elements)
* @throws {TypeError} first argument must be a valid matrix triangle
* @throws {RangeError} second argument must be a nonnegative integer
* @throws {RangeError} third argument must be a nonnegative integer
* @returns {*} result
*/
function zlatrd( uplo, N, nb, A, strideA1, strideA2, offsetA, e, strideE, offsetE, TAU, strideTAU, offsetTAU, W, strideW1, strideW2, offsetW ) {
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( nb < 0 ) {
		throw new RangeError( format( 'invalid argument. Third argument must be a nonnegative integer. Value: `%d`.', nb ) );
	}
	if ( N === 0 ) {
		return;
	}
	return base( uplo, N, nb, A, strideA1, strideA2, offsetA, e, strideE, offsetE, TAU, strideTAU, offsetTAU, W, strideW1, strideW2, offsetW );
}


// EXPORTS //

module.exports = zlatrd;
