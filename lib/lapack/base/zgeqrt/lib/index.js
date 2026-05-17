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
* Compute a blocked QR factorization of a complex M-by-N matrix using the compact WY representation of Q.
*
* @module @stdlib/lapack/base/zgeqrt
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zgeqrt = require( '@stdlib/lapack/base/zgeqrt' );
*
* var A = new Complex128Array( 12 );
* var T = new Complex128Array( 6 );
* var WORK = new Complex128Array( 12 );
*
* zgeqrt( 'column-major', 4, 3, 2, A, 4, T, 2, WORK );
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zgeqrt = require( '@stdlib/lapack/base/zgeqrt' );
*
* var A = new Complex128Array( 12 );
* var T = new Complex128Array( 6 );
* var WORK = new Complex128Array( 12 );
*
* zgeqrt.ndarray( 4, 3, 2, A, 1, 4, 0, T, 1, 2, 0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgeqrt;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgeqrt = main;
} else {
	zgeqrt = tmp;
}


// EXPORTS //

module.exports = zgeqrt;

// exports: { "ndarray": "zgeqrt.ndarray" }
