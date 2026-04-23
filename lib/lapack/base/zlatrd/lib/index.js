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
* Reduce NB rows and columns of a Hermitian matrix to tridiagonal form.
*
* @module @stdlib/lapack/base/zlatrd
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zlatrd = require( '@stdlib/lapack/base/zlatrd' );
*
* var A = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var e = new Float64Array( [ 1.0, 2.0 ] );
* var TAU = new Complex128Array( [ 1.0, 2.0 ] );
* var W = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* zlatrd( 'row-major', 'upper', 2, 2, A, 2, e, 1, TAU, 1, W, 2 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zlatrd = require( '@stdlib/lapack/base/zlatrd' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var e = new Float64Array( [ 1.0, 2.0 ] );
* var TAU = new Float64Array( [ 1.0, 2.0 ] );
* var W = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* zlatrd.ndarray( 'upper', 2, 2, A, 1, 2, 0, e, 1, 0, TAU, 1, 0, W, 1, 2, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlatrd;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlatrd = main;
} else {
	zlatrd = tmp;
}


// EXPORTS //

module.exports = zlatrd;

// exports: { "ndarray": "zlatrd.ndarray" }
