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
* Compute the inverse of a complex symmetric indefinite matrix using the factorization produced by `zsytrf`.
*
* @module @stdlib/lapack/base/zsytri2x
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zsytri2x = require( '@stdlib/lapack/base/zsytri2x' );
*
* var A = new Complex128Array( 1 );
* A.set( [ 5.0, 2.0 ], 0 );
* var IPIV = new Int32Array( [ 0 ] );
* var work = new Complex128Array( 12 );
*
* zsytri2x( 'column-major', 'lower', 1, A, 1, IPIV, 1, 0, work, 1, 1 );
* // A[0] ~ 0.172 - 0.069i
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zsytri2x = require( '@stdlib/lapack/base/zsytri2x' );
*
* var A = new Complex128Array( 1 );
* A.set( [ 5.0, 2.0 ], 0 );
* var IPIV = new Int32Array( [ 0 ] );
* var work = new Complex128Array( 12 );
*
* zsytri2x.ndarray( 'lower', 1, A, 1, 1, 0, IPIV, 1, 0, work, 1, 0, 1 );
* // A[0] ~ 0.172 - 0.069i
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zsytri2x;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsytri2x = main;
} else {
	zsytri2x = tmp;
}


// EXPORTS //

module.exports = zsytri2x;

// exports: { "ndarray": "zsytri2x.ndarray" }
