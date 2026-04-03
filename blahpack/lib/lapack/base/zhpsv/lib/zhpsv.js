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
* Computes the solution to a complex system of linear equations `A * X = B` where A is Hermitian in packed storage.
*
* @param {string} uplo - specifies whether the upper or lower triangular part of the Hermitian matrix A is stored
* @param {NonNegativeInteger} N - order of the matrix A
* @param {NonNegativeInteger} nrhs - number of right-hand side columns in B
* @param {Complex128Array} AP - Hermitian matrix A in packed storage
* @param {integer} strideAP - stride for `AP`
* @param {Int32Array} IPIV - output pivot indices, length N
* @param {integer} strideIPIV - stride for `IPIV`
* @param {Complex128Array} B - input/output N-by-NRHS matrix
* @param {PositiveInteger} LDB - leading dimension of `B`
* @returns {integer} info - 0 if successful
*/
function zhpsv( uplo, N, nrhs, AP, strideAP, IPIV, strideIPIV, B, LDB ) { // eslint-disable-line max-len, max-params
	var oipiv;
	var oap;

	oap = stride2offset( ( N * ( N + 1 ) ) / 2, strideAP ); // eslint-disable-line max-len
	oipiv = stride2offset( N, strideIPIV );
	return base( uplo, N, nrhs, AP, strideAP, oap, IPIV, strideIPIV, oipiv, B, 1, LDB, 0 ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zhpsv;
