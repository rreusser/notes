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
* Compute Cholesky factorization of a Hermitian positive definite matrix (unblocked).
*
* @module @stdlib/lapack/base/zpotf2
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zpotf2 = require( '@stdlib/lapack/base/zpotf2' );
*
* var A = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* zpotf2( 'row-major', 'upper', 2, A, 2 );
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zpotf2 = require( '@stdlib/lapack/base/zpotf2' );
*
* var A = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* zpotf2.ndarray( 'upper', 2, A, 1, 2, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zpotf2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zpotf2 = main;
} else {
	zpotf2 = tmp;
}


// EXPORTS //

module.exports = zpotf2;

// exports: { "ndarray": "zpotf2.ndarray" }
