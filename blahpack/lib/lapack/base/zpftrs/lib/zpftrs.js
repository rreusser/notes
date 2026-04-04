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

var base = require( './base.js' );


// MAIN //

/**
* Solves a system of linear equations `A * X = B` with a Hermitian positive definite matrix `A` in Rectangular Full Packed (RFP) format.
*
* @param {string} transr - specifies the RFP storage format (`'no-transpose'` or `'conjugate-transpose'`)
* @param {string} uplo - specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`)
* @param {NonNegativeInteger} N - order of matrix A
* @param {NonNegativeInteger} nrhs - number of right-hand side columns
* @param {Complex128Array} A - RFP array containing the Cholesky factor
* @param {Complex128Array} B - N-by-NRHS right-hand side matrix (column-major, tightly packed)
* @returns {integer} status code (0 = success)
*/
function zpftrs( transr, uplo, N, nrhs, A, B ) {
	return base( transr, uplo, N, nrhs, A, 1, 0, B, 1, N, 0 );
}


// EXPORTS //

module.exports = zpftrs;
