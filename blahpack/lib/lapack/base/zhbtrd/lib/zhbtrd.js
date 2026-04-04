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
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Reduces a complex Hermitian band matrix to real tridiagonal form by unitary similarity transformation.
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {string} vect - `'none'`, `'initialize'`, or `'update'`
* @param {string} uplo - `'upper'` or `'lower'`
* @param {NonNegativeInteger} N - order of the matrix A
* @param {NonNegativeInteger} kd - number of super- (or sub-) diagonals
* @param {Complex128Array} AB - band matrix (LDAB x N)
* @param {PositiveInteger} LDAB - leading dimension of `AB`
* @param {Float64Array} d - output array for diagonal elements (length N)
* @param {Float64Array} e - output array for off-diagonal elements (length N-1)
* @param {Complex128Array} Q - unitary matrix (LDQ x N)
* @param {PositiveInteger} LDQ - leading dimension of `Q`
* @param {Complex128Array} WORK - workspace (length N)
* @throws {TypeError} first argument must be a valid order
* @returns {integer} status code (0 = success)
*/
function zhbtrd( order, vect, uplo, N, kd, AB, LDAB, d, e, Q, LDQ, WORK ) {
	var sab1;
	var sab2;
	var sq1;
	var sq2;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( order === 'column-major' ) {
		sab1 = 1;
		sab2 = LDAB;
		sq1 = 1;
		sq2 = LDQ;
	} else {
		sab1 = LDAB;
		sab2 = 1;
		sq1 = LDQ;
		sq2 = 1;
	}
	return base( vect, uplo, N, kd, AB, sab1, sab2, 0, d, 1, 0, e, 1, 0, Q, sq1, sq2, 0, WORK, 1, 0 );
}


// EXPORTS //

module.exports = zhbtrd;
