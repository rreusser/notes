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

/* eslint-disable max-len, max-params, camelcase */

'use strict';

// MODULES //

var isLayout = require( '@stdlib/blas/base/assert/is-layout' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Computes the reciprocal pivot growth factor `norm(A)/norm(U)` for a general banded matrix.
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {NonNegativeInteger} N - number of linear equations
* @param {NonNegativeInteger} kl - number of subdiagonals
* @param {NonNegativeInteger} ku - number of superdiagonals
* @param {NonNegativeInteger} ncols - number of columns to process
* @param {Float64Array} AB - original band matrix in band storage
* @param {PositiveInteger} LDAB - leading dimension of `AB`
* @param {Float64Array} AFB - LU factored band matrix
* @param {PositiveInteger} LDAFB - leading dimension of `AFB`
* @throws {TypeError} first argument must be a valid order
* @returns {number} reciprocal pivot growth factor
*/
function dla_gbrpvgrw( order, N, kl, ku, ncols, AB, LDAB, AFB, LDAFB ) {
	var sab1;
	var sab2;
	var sfb1;
	var sfb2;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( order === 'column-major' ) {
		sab1 = 1;
		sab2 = LDAB;
		sfb1 = 1;
		sfb2 = LDAFB;
	} else {
		sab1 = LDAB;
		sab2 = 1;
		sfb1 = LDAFB;
		sfb2 = 1;
	}
	return base( N, kl, ku, ncols, AB, sab1, sab2, 0, AFB, sfb1, sfb2, 0 );
}


// EXPORTS //

module.exports = dla_gbrpvgrw;
