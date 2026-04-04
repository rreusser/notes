/**
* @license Apache-2.0
*
* Copyright (c) 2025 Ricky Reusser.
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
* Reduces a complex Hermitian matrix in packed form to real symmetric tridiagonal form.
*
* @param {string} uplo - specifies whether the upper ('upper') or lower ('lower') triangular part of A is stored
* @param {NonNegativeInteger} N - order of the matrix A
* @param {Complex128Array} AP - packed Hermitian matrix (complex elements)
* @param {Float64Array} d - output array for diagonal elements (length N)
* @param {Float64Array} e - output array for off-diagonal elements (length N-1)
* @param {Complex128Array} TAU - output array for reflector scalars (length N-1, complex elements)
* @throws {TypeError} first argument must be a valid matrix triangle
* @returns {integer} status code (0 = success)
*/
function zhptrd( uplo, N, AP, d, e, TAU ) {
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	return base( uplo, N, AP, 1, 0, d, 1, 0, e, 1, 0, TAU, 1, 0 );
}


// EXPORTS //

module.exports = zhptrd;
