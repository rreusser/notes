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
var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var base = require( './base.js' );


// MAIN //

/**
* Computes all eigenvalues and, optionally, eigenvectors of a real symmetric band matrix.
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {string} jobz - `'no-vectors'` or `'compute-vectors'`
* @param {string} uplo - `'upper'` or `'lower'`
* @param {NonNegativeInteger} N - order of the matrix A
* @param {NonNegativeInteger} kd - number of super- (or sub-) diagonals
* @param {Float64Array} AB - band matrix in band storage
* @param {PositiveInteger} LDAB - leading dimension of `AB`
* @param {Float64Array} w - output array for eigenvalues
* @param {integer} strideW - `w` stride length
* @param {Float64Array} Z - output matrix for eigenvectors
* @param {PositiveInteger} LDZ - leading dimension of `Z`
* @param {Float64Array} WORK - workspace array
* @param {integer} strideWORK - `WORK` stride length
* @throws {TypeError} first argument must be a valid order
* @returns {integer} status code (0 = success)
*/
function dsbev( order, jobz, uplo, N, kd, AB, LDAB, w, strideW, Z, LDZ, WORK, strideWORK ) {
	var sab1;
	var sab2;
	var sz1;
	var sz2;
	var ow;
	var owk;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. Third argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( order === 'column-major' ) {
		sab1 = 1;
		sab2 = LDAB;
		sz1 = 1;
		sz2 = LDZ;
	} else {
		sab1 = LDAB;
		sab2 = 1;
		sz1 = LDZ;
		sz2 = 1;
	}
	ow = stride2offset( N, strideW );
	owk = stride2offset( Math.max( 1, ( 3 * N ) - 2 ), strideWORK );
	return base( jobz, uplo, N, kd, AB, sab1, sab2, 0, w, strideW, ow, Z, sz1, sz2, 0, WORK, strideWORK, owk );
}


// EXPORTS //

module.exports = dsbev;
