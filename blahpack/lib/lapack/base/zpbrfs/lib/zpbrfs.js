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

var base = require( './base.js' );


// MAIN //

/**
* Improves the computed solution to a complex system `A * X = B` where `A` is Hermitian positive definite band, and provides error bounds.
*
* @param {string} uplo - specifies whether 'upper' or 'lower' triangle is stored
* @param {NonNegativeInteger} N - order of matrix A
* @param {NonNegativeInteger} kd - number of superdiagonals (or subdiagonals) of A
* @param {NonNegativeInteger} nrhs - number of right-hand side columns
* @param {Complex128Array} AB - original Hermitian band matrix in band storage
* @param {PositiveInteger} LDAB - leading dimension of AB (in complex elements)
* @param {Complex128Array} AFB - Cholesky-factored band matrix (from zpbtrf)
* @param {PositiveInteger} LDAFB - leading dimension of AFB (in complex elements)
* @param {Complex128Array} B - right-hand side matrix (column-major)
* @param {PositiveInteger} LDB - leading dimension of B (in complex elements)
* @param {Complex128Array} X - solution matrix (improved on exit)
* @param {PositiveInteger} LDX - leading dimension of X (in complex elements)
* @param {Float64Array} FERR - output forward error bounds (length nrhs)
* @param {Float64Array} BERR - output backward error bounds (length nrhs)
* @param {Complex128Array} WORK - complex workspace array (length >= 2*N)
* @param {Float64Array} RWORK - real workspace array (length >= N)
* @returns {integer} info - 0 if successful
*/
function zpbrfs( uplo, N, kd, nrhs, AB, LDAB, AFB, LDAFB, B, LDB, X, LDX, FERR, BERR, WORK, RWORK ) {
	return base( uplo, N, kd, nrhs, AB, 1, LDAB, 0, AFB, 1, LDAFB, 0, B, 1, LDB, 0, X, 1, LDX, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zpbrfs;
