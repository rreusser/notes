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
* Chases a 1-by-1 shift bulge in a complex matrix pencil down a single position.
*
* @module @stdlib/lapack/base/zlaqz1
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zlaqz1 = require( '@stdlib/lapack/base/zlaqz1' );
*
* var A = new Complex128Array( 25 );
* var B = new Complex128Array( 25 );
* var Q = new Complex128Array( 25 );
* var Z = new Complex128Array( 25 );
*
* // Initialize B, Q, Z to identity:
* var i;
* for ( i = 0; i < 5; i++ ) {
*     B.set( [ 1.0, 0.0 ], ( i * 5 ) + i );
*     Q.set( [ 1.0, 0.0 ], ( i * 5 ) + i );
*     Z.set( [ 1.0, 0.0 ], ( i * 5 ) + i );
* }
*
* zlaqz1( 'column-major', true, true, 1, 0, 4, 4, A, 5, B, 5, 5, 0, Q, 5, 5, 0, Z, 5 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlaqz1;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlaqz1 = main;
} else {
	zlaqz1 = tmp;
}


// EXPORTS //

module.exports = zlaqz1;

// exports: { "ndarray": "zlaqz1.ndarray" }
