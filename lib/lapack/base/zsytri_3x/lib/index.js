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
* Compute the inverse of a complex symmetric indefinite matrix using the factorization produced by `zsytrf_rk`.
*
* @module @stdlib/lapack/base/zsytri_3x
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zsytri3x = require( '@stdlib/lapack/base/zsytri_3x' );
*
* var A = new Complex128Array( [ 4.0, 1.0 ] );
* var e = new Complex128Array( 1 );
* var IPIV = new Int32Array( [ 0 ] );
* var work = new Complex128Array( 6 );
*
* zsytri3x( 'column-major', 'lower', 1, A, 1, e, 1, IPIV, 1, 0, work, 1, 1 );
* // A[0] ~ 0.235 - 0.0588i
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zsytri3x = require( '@stdlib/lapack/base/zsytri_3x' );
*
* var A = new Complex128Array( [ 4.0, 1.0 ] );
* var e = new Complex128Array( 1 );
* var IPIV = new Int32Array( [ 0 ] );
* var work = new Complex128Array( 6 );
*
* zsytri3x.ndarray( 'lower', 1, A, 1, 1, 0, e, 1, 0, IPIV, 1, 0, work, 1, 0, 1 );
* // A[0] ~ 0.235 - 0.0588i
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zsytri3x;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsytri3x = main;
} else {
	zsytri3x = tmp;
}


// EXPORTS //

module.exports = zsytri3x;

// exports: { "ndarray": "zsytri3x.ndarray" }
