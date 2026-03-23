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

var dpotrf = require( '../../dpotrf/lib/base.js' );
var dpotrs = require( '../../dpotrs/lib/base.js' );


// MAIN //

/**
* Computes the solution to a real system of linear equations A*X = B,.
* where A is an N-by-N symmetric positive definite matrix and X and B
* are N-by-NRHS matrices.
*
* The Cholesky decomposition is used to factor A as:
*   A = U^T _ U,  if uplo = 'upper', or
_   A = L _ L^T,  if uplo = 'lower',
* where U is upper triangular and L is lower triangular. The factored
* form of A is then used to solve the system A*X = B.
*
* @private
* @param {string} uplo - 'U' for upper triangle, 'L' for lower triangle
* @param {NonNegativeInteger} N - order of matrix A
* @param {NonNegativeInteger} nrhs - number of right-hand side columns
* @param {Float64Array} A - input/output matrix; on exit, the Cholesky factor
* @param {integer} strideA1 - stride of the first dimension of A
* @param {integer} strideA2 - stride of the second dimension of A
* @param {NonNegativeInteger} offsetA - index offset for A
* @param {Float64Array} B - input/output N-by-NRHS matrix; on exit, the solution X
* @param {integer} strideB1 - stride of the first dimension of B
* @param {integer} strideB2 - stride of the second dimension of B
* @param {NonNegativeInteger} offsetB - index offset for B
* @returns {integer} info - 0 if successful, k>0 if A is not positive definite
*/
function dposv( uplo, N, nrhs, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB ) {
	var info;

	if ( N === 0 || nrhs === 0 ) {
		return 0;
	}

	// Compute the Cholesky factorization A = U^T*U or A = L*L^T.
	info = dpotrf( uplo, N, A, strideA1, strideA2, offsetA );

	if ( info === 0 ) {
		// Solve the system using the factorization.
		dpotrs( uplo, N, nrhs, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB );
	}

	return info;
}


// EXPORTS //

module.exports = dposv;
