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
* Reduce a pair of complex matrices to generalized upper Hessenberg form.
*
* @module @stdlib/lapack/base/zgghrd
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zgghrd = require( '@stdlib/lapack/base/zgghrd' );
*
* var A = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var B = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var Q = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var Z = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* zgghrd( 'row-major', 'none', 'none', 2, 1, 2, A, 2, B, 2, Q, 2, Z, 2 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zgghrd = require( '@stdlib/lapack/base/zgghrd' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var B = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var Q = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var Z = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* zgghrd.ndarray( 'none', 'none', 2, 1, 2, A, 1, 2, 0, B, 1, 2, 0, Q, 1, 2, 0, Z, 1, 2, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgghrd;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgghrd = main;
} else {
	zgghrd = tmp;
}


// EXPORTS //

module.exports = zgghrd;

// exports: { "ndarray": "zgghrd.ndarray" }
