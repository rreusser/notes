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
* Computes the solution to a real system of linear equations A.
*
* @param {string} uplo - `'upper'` or `'lower'`
* @param {NonNegativeInteger} N - number of linear equations / order of A
* @param {NonNegativeInteger} kd - number of super/sub-diagonals of A
* @param {NonNegativeInteger} nrhs - number of right hand sides (columns of B)
* @param {Float64Array} AB - on entry, the upper or lower triangle of A in band storage; on exit, the triangular factor U or L
* @param {integer} strideAB1 - stride of the first dimension of AB
* @param {integer} strideAB2 - stride of the second dimension of AB
* @param {NonNegativeInteger} offsetAB - starting index for AB
* @param {Float64Array} B - on entry, the RHS matrix B; on exit, the solution matrix X
* @param {integer} strideB1 - stride of the first dimension of B
* @param {integer} strideB2 - stride of the second dimension of B
* @param {NonNegativeInteger} offsetB - starting index for B
* @throws {TypeError} first argument must be a valid matrix triangle
* @throws {RangeError} second argument must be a nonnegative integer
* @throws {RangeError} fourth argument must be a nonnegative integer
* @returns {integer} info - 0 if successful, >0 if the leading minor of order info is not positive definite
*/
function dpbsv( uplo, N, kd, nrhs, AB, strideAB1, strideAB2, offsetAB, B, strideB1, strideB2, offsetB ) {
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( nrhs < 0 ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be a nonnegative integer. Value: `%d`.', nrhs ) );
	}
	if ( N === 0 ) {
		return 0;
	}
	return base( uplo, N, kd, nrhs, AB, strideAB1, strideAB2, offsetAB, B, strideB1, strideB2, offsetB );
}


// EXPORTS //

module.exports = dpbsv;
