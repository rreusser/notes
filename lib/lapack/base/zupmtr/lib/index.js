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
* Overwrites a general complex M-by-N matrix C with Q*C, C*Q, Q^H*C, or C*Q^H,
* where Q is a complex unitary matrix from ZHPTRD (packed storage).
*
* @module @stdlib/lapack/base/zupmtr
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zupmtr = require( '@stdlib/lapack/base/zupmtr' );
*
* // After calling zhptrd with UPLO='upper' on a 4x4 Hermitian matrix:
* var AP = new Complex128Array( [ ... ] );  // packed reflectors
* var TAU = new Complex128Array( [ ... ] ); // scalar factors
* var C = new Complex128Array( 16 );        // 4x4 identity
* var WORK = new Complex128Array( 4 );
*
* zupmtr( 'left', 'upper', 'no-transpose', 4, 4, AP, TAU, C, 4, WORK );
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zupmtr = require( '@stdlib/lapack/base/zupmtr' );
*
* var AP = new Complex128Array( [ ... ] );
* var TAU = new Complex128Array( [ ... ] );
* var C = new Complex128Array( 16 );
* var WORK = new Complex128Array( 4 );
*
* zupmtr.ndarray( 'left', 'upper', 'no-transpose', 4, 4, AP, 1, 0, TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zupmtr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zupmtr = main;
} else {
	zupmtr = tmp;
}


// EXPORTS //

module.exports = zupmtr;

// exports: { "ndarray": "zupmtr.ndarray" }
