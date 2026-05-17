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
* Compute the bounded Bunch-Kaufman (rook) factorization of a complex symmetric indefinite matrix in `_rk` storage format (blocked algorithm).
*
* @module @stdlib/lapack/base/zsytrf_rk
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zsytrfRk = require( '@stdlib/lapack/base/zsytrf_rk' );
*
* var A = new Complex128Array( [ 4.0, 0.5, 1.0, 2.0, 0.0, 0.0, 5.0, -0.5 ] );
* var e = new Complex128Array( 2 );
* var IPIV = new Int32Array( 2 );
*
* zsytrfRk( 'column-major', 'lower', 2, A, 2, e, 1, IPIV, 1 );
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zsytrfRk = require( '@stdlib/lapack/base/zsytrf_rk' );
*
* var A = new Complex128Array( [ 4.0, 0.5, 1.0, 2.0, 0.0, 0.0, 5.0, -0.5 ] );
* var e = new Complex128Array( 2 );
* var IPIV = new Int32Array( 2 );
*
* zsytrfRk.ndarray( 'lower', 2, A, 1, 2, 0, e, 1, 0, IPIV, 1, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zsytrfrk;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsytrfrk = main;
} else {
	zsytrfrk = tmp;
}


// EXPORTS //

module.exports = zsytrfrk;

// exports: { "ndarray": "zsytrf_rk.ndarray" }
