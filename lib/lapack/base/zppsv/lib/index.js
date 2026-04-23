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
* Compute the solution to a complex system of linear equations A*X=B where A is Hermitian positive definite in packed storage.
*
* @module @stdlib/lapack/base/zppsv
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zppsv = require( '@stdlib/lapack/base/zppsv' );
*
* var AP = new Complex128Array( [ 4.0, 0.0, 1.0, -2.0, 10.0, 0.0 ] );
* var B = new Complex128Array( [ 8.0, 4.0, 12.0, -6.0 ] );
*
* zppsv( 'column-major', 'upper', 2, 1, AP, B, 2 );
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zppsv = require( '@stdlib/lapack/base/zppsv' );
*
* var AP = new Complex128Array( [ 4.0, 0.0, 1.0, -2.0, 10.0, 0.0 ] );
* var B = new Complex128Array( [ 8.0, 4.0, 12.0, -6.0 ] );
*
* zppsv.ndarray( 'upper', 2, 1, AP, 1, 0, B, 1, 2, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zppsv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zppsv = main;
} else {
	zppsv = tmp;
}


// EXPORTS //

module.exports = zppsv;

// exports: { "ndarray": "zppsv.ndarray" }
