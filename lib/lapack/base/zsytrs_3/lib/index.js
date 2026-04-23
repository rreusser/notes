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
* Solve a system `A*X = B` for a complex symmetric (NOT Hermitian) matrix `A` using the factorization computed by `zsytrf_rk`.
*
* @module @stdlib/lapack/base/zsytrs_3
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zsytrs3 = require( '@stdlib/lapack/base/zsytrs_3' );
*
* var A = new Complex128Array( [ 4.0, 1.0, 0.0, 0.0, 0.0, 0.0, 5.0, -1.0 ] );
* var e = new Complex128Array( [ 0.0, 0.0, 0.0, 0.0 ] );
* var IPIV = new Int32Array( [ 0, 1 ] );
* var B = new Complex128Array( [ 4.0, 1.0, 5.0, -1.0 ] );
*
* zsytrs3( 'column-major', 'lower', 2, 1, A, 2, e, 1, IPIV, 1, B, 2 );
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zsytrs3 = require( '@stdlib/lapack/base/zsytrs_3' );
*
* var A = new Complex128Array( [ 4.0, 1.0, 0.0, 0.0, 0.0, 0.0, 5.0, -1.0 ] );
* var e = new Complex128Array( [ 0.0, 0.0, 0.0, 0.0 ] );
* var IPIV = new Int32Array( [ 0, 1 ] );
* var B = new Complex128Array( [ 4.0, 1.0, 5.0, -1.0 ] );
*
* zsytrs3.ndarray( 'lower', 2, 1, A, 1, 2, 0, e, 1, 0, IPIV, 1, 0, B, 1, 2, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zsytrs3;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsytrs3 = main;
} else {
	zsytrs3 = tmp;
}


// EXPORTS //

module.exports = zsytrs3;

// exports: { "ndarray": "zsytrs3.ndarray" }
