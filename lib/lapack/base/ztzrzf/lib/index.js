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
* Reduces a complex M-by-N (M <= N) upper trapezoidal matrix to upper triangular form via the unitary RZ factorization (blocked driver).
*
* @module @stdlib/lapack/base/ztzrzf
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var ztzrzf = require( '@stdlib/lapack/base/ztzrzf' );
*
* var A = new Complex128Array( [ 4.0, 0.5, 0.0, 0.0, 0.0, 0.0, 1.0, -0.2, 5.0, 0.3, 0.0, 0.0, 2.0, 0.3, 1.0, 0.1, 6.0, 0.4, 3.0, 0.1, 2.0, 0.2, 1.0, -0.2, 1.0, -0.4, 4.0, -0.5, 2.0, 0.6 ] );
* var TAU = new Complex128Array( 3 );
* var WORK = new Complex128Array( 96 );
*
* ztzrzf( 'column-major', 3, 5, A, 3, TAU, 1, WORK, 1 );
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var ztzrzf = require( '@stdlib/lapack/base/ztzrzf' );
*
* var A = new Complex128Array( [ 4.0, 0.5, 0.0, 0.0, 0.0, 0.0, 1.0, -0.2, 5.0, 0.3, 0.0, 0.0, 2.0, 0.3, 1.0, 0.1, 6.0, 0.4, 3.0, 0.1, 2.0, 0.2, 1.0, -0.2, 1.0, -0.4, 4.0, -0.5, 2.0, 0.6 ] );
* var TAU = new Complex128Array( 3 );
* var WORK = new Complex128Array( 96 );
*
* ztzrzf.ndarray( 3, 5, A, 1, 3, 0, TAU, 1, 0, WORK, 1, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var ztzrzf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztzrzf = main;
} else {
	ztzrzf = tmp;
}


// EXPORTS //

module.exports = ztzrzf;

// exports: { "ndarray": "ztzrzf.ndarray" }
