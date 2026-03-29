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
* Computes the reciprocal pivot growth factor `norm(A)/norm(U)` for a complex general matrix.
*
* @param {NonNegativeInteger} N - number of rows
* @param {NonNegativeInteger} ncols - number of columns to process
* @param {Complex128Array} A - input matrix A (column-major, dimension N x ncols)
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Complex128Array} AF - factored matrix AF (column-major, dimension N x ncols)
* @param {PositiveInteger} LDAF - leading dimension of `AF`
* @returns {number} reciprocal pivot growth factor
*/
function zla_gerpvgrw( N, ncols, A, LDA, AF, LDAF ) {
	return base( N, ncols, A, 1, LDA, 0, AF, 1, LDAF, 0 );
}


// EXPORTS //

module.exports = zla_gerpvgrw;
