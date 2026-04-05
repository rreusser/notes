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

var isLayout = require( '@stdlib/blas/base/assert/is-layout' );
var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var base = require( './base.js' );


// MAIN //

/**
* Computes all eigenvalues and, optionally, eigenvectors of a complex generalized Hermitian-definite banded eigenproblem A_x = lambda_B_x.
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {string} jobz - `'no-vectors'` or `'compute-vectors'`
* @param {string} uplo - `'upper'` or `'lower'`
* @param {NonNegativeInteger} N - order of matrices A and B
* @param {integer} ka - number of super- (or sub-) diagonals of A
* @param {integer} kb - number of super- (or sub-) diagonals of B
* @param {Complex128Array} AB - band matrix A in band storage
* @param {PositiveInteger} LDAB - leading dimension of `AB`
* @param {Complex128Array} BB - band matrix B in band storage
* @param {PositiveInteger} LDBB - leading dimension of `BB`
* @param {Float64Array} w - output array for eigenvalues
* @param {integer} strideW - `w` stride length
* @param {Complex128Array} Z - output matrix for eigenvectors
* @param {PositiveInteger} LDZ - leading dimension of `Z`
* @param {Complex128Array} WORK - complex workspace array
* @param {integer} strideWORK - `WORK` stride length (in complex elements)
* @param {Float64Array} RWORK - real workspace array
* @param {integer} strideRWORK - `RWORK` stride length
* @throws {TypeError} first argument must be a valid order
* @returns {integer} status code (0 = success)
*/
function zhbgv( order, jobz, uplo, N, ka, kb, AB, LDAB, BB, LDBB, w, strideW, Z, LDZ, WORK, strideWORK, RWORK, strideRWORK ) {
	var sbb1;
	var sbb2;
	var sab1;
	var sab2;
	var ork;
	var owk;
	var sz1;
	var sz2;
	var ow;

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
		sbb1 = 1;
		sbb2 = LDBB;
		sz1 = 1;
		sz2 = LDZ;
	} else {
		sab1 = LDAB;
		sab2 = 1;
		sbb1 = LDBB;
		sbb2 = 1;
		sz1 = LDZ;
		sz2 = 1;
	}
	ow = stride2offset( N, strideW );
	owk = stride2offset( Math.max( 1, N ), strideWORK );
	ork = stride2offset( Math.max( 1, 3 * N ), strideRWORK );
	return base( jobz, uplo, N, ka, kb, AB, sab1, sab2, 0, BB, sbb1, sbb2, 0, w, strideW, ow, Z, sz1, sz2, 0, WORK, strideWORK, owk, RWORK, strideRWORK, ork );
}


// EXPORTS //

module.exports = zhbgv;
