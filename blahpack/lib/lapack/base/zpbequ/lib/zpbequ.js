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
* Computes row and column scalings intended to equilibrate a complex Hermitian positive definite band matrix and reduce its condition number.
*
* @param {string} uplo - specifies whether the upper or lower triangle is stored
* @param {NonNegativeInteger} N - order of the matrix A
* @param {NonNegativeInteger} kd - number of superdiagonals or subdiagonals
* @param {Complex128Array} AB - input band matrix in band storage
* @param {PositiveInteger} LDAB - leading dimension of `AB`
* @param {Float64Array} s - output scale factors
* @param {integer} strideS - stride for `s`
* @returns {Object} result with `info`, `scond`, and `amax` properties
*/
function zpbequ( uplo, N, kd, AB, LDAB, s, strideS ) {
	var os;

	os = stride2offset( N, strideS );
	return base( uplo, N, kd, AB, 1, LDAB, 0, s, strideS, os );
}


// EXPORTS //

module.exports = zpbequ;
