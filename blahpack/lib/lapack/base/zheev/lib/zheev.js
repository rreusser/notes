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
var base = require( './base.js' );


// MAIN //

/**
* Computes all eigenvalues and, optionally, eigenvectors of a complex Hermitian.
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {string} jobz - TODO
* @param {string} uplo - TODO
* @param {NonNegativeInteger} N - TODO
* @param {Complex128Array} A - input matrix
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} w - input array
* @param {integer} strideW - `w` stride length
* @param {Complex128Array} WORK - input array
* @param {integer} strideWORK - `WORK` stride length
* @param {integer} lwork - TODO
* @param {Float64Array} RWORK - input array
* @param {integer} strideRWORK - `RWORK` stride length
* @throws {TypeError} first argument must be a valid order
* @returns {*} result
*/
function zheev( order, jobz, uplo, N, A, LDA, w, strideW, WORK, strideWORK, lwork, RWORK, strideRWORK ) {
	var sa1;
	var sa2;
	var ow;
	var ow;
	var or;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( order === 'column-major' ) {
		sa1 = 1;
		sa2 = LDA;
	} else {
		sa1 = LDA;
		sa2 = 1;
	}
	ow = stride2offset( N, strideW );
	ow = stride2offset( N, strideWORK );
	or = stride2offset( N, strideRWORK );
	return base( jobz, uplo, N, A, sa1, sa2, 0, w, strideW, ow, WORK, strideWORK, ow, lwork, RWORK, strideRWORK, or );
}


// EXPORTS //

module.exports = zheev;
