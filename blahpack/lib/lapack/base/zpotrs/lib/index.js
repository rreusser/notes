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

/**
* Solves a Hermitian positive definite system using Cholesky factorization (complex double-precision).
*
* @module @stdlib/lapack/base/zpotrs
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zpotrs = require( '@stdlib/lapack/base/zpotrs' );
*
* var A = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var B = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* zpotrs( 'row-major', 'upper', 2, 1, A, 2, B, 2 );
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zpotrs = require( '@stdlib/lapack/base/zpotrs' );
*
* var A = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var B = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* zpotrs.ndarray( 'upper', 2, 1, A, 1, 2, 0, B, 1, 2, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zpotrs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zpotrs = main;
} else {
	zpotrs = tmp;
}


// EXPORTS //

module.exports = zpotrs;

// exports: { "ndarray": "zpotrs.ndarray" }
