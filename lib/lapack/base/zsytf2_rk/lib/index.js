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
* Factorize a complex symmetric matrix using the bounded Bunch-Kaufman (rook) diagonal pivoting method (`_rk` format).
*
* @module @stdlib/lapack/base/zsytf2_rk
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zsytf2rk = require( '@stdlib/lapack/base/zsytf2_rk' );
*
* var A = new Complex128Array( [ 4.0, 0.5, 0.0, 0.0, 0.0, 0.0, -3.0, 1.0 ] );
* var e = new Complex128Array( 2 );
* var ipiv = new Int32Array( 2 );
*
* var info = zsytf2rk( 'column-major', 'lower', 2, A, 2, e, 1, ipiv, 1, 0 );
* // returns 0
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zsytf2rk = require( '@stdlib/lapack/base/zsytf2_rk' );
*
* var A = new Complex128Array( [ 4.0, 0.5, 0.0, 0.0, 0.0, 0.0, -3.0, 1.0 ] );
* var e = new Complex128Array( 2 );
* var ipiv = new Int32Array( 2 );
*
* var info = zsytf2rk.ndarray( 'lower', 2, A, 1, 2, 0, e, 1, 0, ipiv, 1, 0 );
* // returns 0
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zsytf2rk;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsytf2rk = main;
} else {
	zsytf2rk = tmp;
}


// EXPORTS //

module.exports = zsytf2rk;

// exports: { "ndarray": "zsytf2rk.ndarray" }
