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

'use strict';

// MODULES //

var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Computes the inverse of a real symmetric positive definite matrix in packed storage using the Cholesky factorization.
*
* @param {string} uplo - specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`)
* @param {NonNegativeInteger} N - order of the matrix
* @param {Float64Array} AP - packed triangular matrix
* @param {integer} stride - stride length for `AP`
* @param {NonNegativeInteger} offset - starting index for `AP`
* @throws {TypeError} First argument must be a valid matrix triangle.
* @returns {integer} info - `0` if successful; `k > 0` if the `k`-th diagonal element is zero (1-based)
*/
function dpptri( uplo, N, AP, stride, offset ) {
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	return base( uplo, N, AP, stride, offset );
}


// EXPORTS //

module.exports = dpptri;
