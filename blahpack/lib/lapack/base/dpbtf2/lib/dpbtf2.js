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
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Computes the Cholesky factorization of a real symmetric positive definite.
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {string} uplo - TODO
* @param {NonNegativeInteger} N - TODO
* @param {NonNegativeInteger} kd - TODO
* @param {Float64Array} AB - input matrix
* @param {PositiveInteger} LDAB - leading dimension of `AB`
* @throws {TypeError} first argument must be a valid order
* @returns {*} result
*/
function dpbtf2( order, uplo, N, kd, AB, LDAB ) {
	var sa1;
	var sa2;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( order === 'column-major' ) {
		sa1 = 1;
		sa2 = LDAB;
	} else {
		sa1 = LDAB;
		sa2 = 1;
	}
	return base( uplo, N, kd, AB, sa1, sa2, 0 );
}


// EXPORTS //

module.exports = dpbtf2;
