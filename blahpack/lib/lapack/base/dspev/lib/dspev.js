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

var isLayout = require( '@stdlib/blas/base/assert/is-layout' );
var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Computes all eigenvalues and, optionally, eigenvectors of a real symmetric matrix in packed storage.
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {string} jobz - `'none'` (eigenvalues only) or `'compute'` (eigenvalues + eigenvectors)
* @param {string} uplo - `'upper'` or `'lower'`
* @param {NonNegativeInteger} N - order of the matrix A
* @param {Float64Array} AP - packed symmetric matrix (length N*(N+1)/2)
* @param {Float64Array} w - output array for eigenvalues (length N)
* @param {Float64Array} Z - output eigenvector matrix (N x N)
* @param {PositiveInteger} LDZ - leading dimension of Z
* @param {Float64Array} WORK - workspace array (length >= 3*N)
* @throws {TypeError} first argument must be a valid order
* @throws {TypeError} second argument must be a valid job type
* @throws {TypeError} third argument must be a valid matrix triangle
* @returns {integer} info - 0 if successful
*/
function dspev( order, jobz, uplo, N, AP, w, Z, LDZ, WORK ) {
	var sz1;
	var sz2;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( jobz !== 'none' && jobz !== 'compute' ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid job type. Value: `%s`.', jobz ) );
	}
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. Third argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( order === 'column-major' ) {
		sz1 = 1;
		sz2 = LDZ;
	} else {
		sz1 = LDZ;
		sz2 = 1;
	}
	return base( jobz, uplo, N, AP, 1, 0, w, 1, 0, Z, sz1, sz2, 0, WORK, 1, 0 );
}


// EXPORTS //

module.exports = dspev;
