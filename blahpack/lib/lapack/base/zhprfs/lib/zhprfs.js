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
var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var format = require( '@stdlib/string/format' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var base = require( './base.js' );


// MAIN //

/**
* Improves the computed solution to a complex Hermitian system of linear.
* equations A * X = B where A is stored in packed format, and provides
* error bounds and backward error estimates.
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {string} uplo - specifies the triangle ('upper' or 'lower')
* @param {NonNegativeInteger} N - order of the matrix
* @param {NonNegativeInteger} nrhs - number of right-hand sides
* @param {Complex128Array} AP - original Hermitian packed matrix
* @param {Complex128Array} AFP - factored packed matrix from zhptrf
* @param {Int32Array} IPIV - pivot indices from zhptrf
* @param {Complex128Array} B - right-hand side matrix
* @param {PositiveInteger} LDB - leading dimension of B
* @param {Complex128Array} X - solution matrix (improved on exit)
* @param {PositiveInteger} LDX - leading dimension of X
* @param {Float64Array} FERR - output forward error bounds
* @param {Float64Array} BERR - output backward error bounds
* @throws {TypeError} first argument must be a valid order
* @throws {TypeError} second argument must be a valid matrix triangle
* @returns {integer} status code (0 = success)
*/
function zhprfs( order, uplo, N, nrhs, AP, AFP, IPIV, B, LDB, X, LDX, FERR, BERR ) {
	var RWORK;
	var WORK;
	var sb1;
	var sb2;
	var sx1;
	var sx2;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( order === 'column-major' ) {
		sb1 = 1;
		sb2 = LDB;
		sx1 = 1;
		sx2 = LDX;
	} else {
		sb1 = LDB;
		sb2 = 1;
		sx1 = LDX;
		sx2 = 1;
	}
	WORK = new Complex128Array( 2 * N );
	RWORK = new Float64Array( N );
	return base( uplo, N, nrhs, AP, 1, 0, AFP, 1, 0, IPIV, 1, 0, B, sb1, sb2, 0, X, sx1, sx2, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
}


// EXPORTS //

module.exports = zhprfs;
