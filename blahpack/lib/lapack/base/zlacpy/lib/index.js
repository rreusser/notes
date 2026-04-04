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
* Copy all or part of a complex matrix.
*
* @module @stdlib/lapack/base/zlacpy
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zlacpy = require( '@stdlib/lapack/base/zlacpy' );
*
* var A = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var B = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* zlacpy( 'row-major', 'upper', 2, 2, A, 2, B, 2 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zlacpy = require( '@stdlib/lapack/base/zlacpy' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var B = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* zlacpy.ndarray( 'upper', 2, 2, A, 1, 2, 0, B, 1, 2, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlacpy;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlacpy = main;
} else {
	zlacpy = tmp;
}


// EXPORTS //

module.exports = zlacpy;

// exports: { "ndarray": "zlacpy.ndarray" }
