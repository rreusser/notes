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
* Compute the inverse of a complex Hermitian indefinite matrix using the factorization produced by `zhetrf_rk`.
*
* @module @stdlib/lapack/base/zhetri_3x
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zhetri3x = require( '@stdlib/lapack/base/zhetri_3x' );
*
* var A = new Complex128Array( [ 5.0, 0.0 ] );
* var e = new Complex128Array( 1 );
* var IPIV = new Int32Array( [ 0 ] );
* var work = new Complex128Array( 6 );
*
* zhetri3x( 'column-major', 'lower', 1, A, 1, e, 1, IPIV, 1, 0, work, 1, 1 );
* // A[0] ~ 0.2 + 0i
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zhetri3x = require( '@stdlib/lapack/base/zhetri_3x' );
*
* var A = new Complex128Array( [ 5.0, 0.0 ] );
* var e = new Complex128Array( 1 );
* var IPIV = new Int32Array( [ 0 ] );
* var work = new Complex128Array( 6 );
*
* zhetri3x.ndarray( 'lower', 1, A, 1, 1, 0, e, 1, 0, IPIV, 1, 0, work, 1, 0, 1 );
* // A[0] ~ 0.2 + 0i
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhetri3x;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhetri3x = main;
} else {
	zhetri3x = tmp;
}


// EXPORTS //

module.exports = zhetri3x;

// exports: { "ndarray": "zhetri3x.ndarray" }
