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

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* Computes selected eigenvalues and, optionally, eigenvectors of a real generalized symmetric-definite banded eigenproblem A_x = lambda_B_x.
*
* @param {string} jobz - `'no-vectors'` or `'compute-vectors'`
* @param {string} range - `'all'`, `'value'`, or `'index'`
* @param {string} uplo - `'upper'` or `'lower'`
* @param {NonNegativeInteger} N - order of matrices A and B
* @param {integer} ka - number of super- (or sub-) diagonals of A
* @param {integer} kb - number of super- (or sub-) diagonals of B
* @param {Float64Array} AB - band matrix A in band storage
* @param {PositiveInteger} LDAB - leading dimension of `AB`
* @param {Float64Array} BB - band matrix B in band storage
* @param {PositiveInteger} LDBB - leading dimension of `BB`
* @param {Float64Array} Q - output transformation matrix (N-by-N)
* @param {PositiveInteger} LDQ - leading dimension of `Q`
* @param {number} vl - lower bound of eigenvalue interval
* @param {number} vu - upper bound of eigenvalue interval
* @param {integer} il - index of smallest eigenvalue (1-based)
* @param {integer} iu - index of largest eigenvalue (1-based)
* @param {number} abstol - absolute tolerance
* @param {Object} out - output object; out.M set to number of eigenvalues found
* @param {Float64Array} w - output eigenvalues
* @param {integer} strideW - stride for `w`
* @param {Float64Array} Z - output eigenvectors
* @param {PositiveInteger} LDZ - leading dimension of `Z`
* @param {Float64Array} WORK - workspace
* @param {integer} strideWORK - stride for `WORK`
* @param {Int32Array} IWORK - integer workspace
* @param {integer} strideIWORK - stride for `IWORK`
* @param {Int32Array} IFAIL - output failure indices
* @param {integer} strideIFAIL - stride for `IFAIL`
* @returns {integer} info - 0 if successful
*/
function dsbgvx( jobz, range, uplo, N, ka, kb, AB, LDAB, BB, LDBB, Q, LDQ, vl, vu, il, iu, abstol, out, w, strideW, Z, LDZ, WORK, strideWORK, IWORK, strideIWORK, IFAIL, strideIFAIL ) { // eslint-disable-line max-len, max-params
	var oifail;
	var oiwork;
	var owork;
	var ow;

	ow = stride2offset( N, strideW );
	owork = stride2offset( N, strideWORK );
	oiwork = stride2offset( N, strideIWORK );
	oifail = stride2offset( N, strideIFAIL );
	return base( jobz, range, uplo, N, ka, kb, AB, 1, LDAB, 0, BB, 1, LDBB, 0, Q, 1, LDQ, 0, vl, vu, il, iu, abstol, out, w, strideW, ow, Z, 1, LDZ, 0, WORK, strideWORK, owork, IWORK, strideIWORK, oiwork, IFAIL, strideIFAIL, oifail ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dsbgvx;
