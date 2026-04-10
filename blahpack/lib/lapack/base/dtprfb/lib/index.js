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
* Apply a real triangular-pentagonal block reflector to a matrix.
*
* @module @stdlib/lapack/base/dtprfb
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dtprfb = require( '@stdlib/lapack/base/dtprfb' );
*
* var V = new Float64Array( [ 1.0, 0.0, 0.0, 0.2, 0.3, 1.0, 0.0, 1.0 ] );
* var T = new Float64Array( [ 1.2, 0.0, -0.3, 0.8 ] );
* var A = new Float64Array( [ 1.0, 4.0, 2.0, 5.0, 3.0, 6.0 ] );
* var B = new Float64Array( [ 7.0, 10.0, 13.0, 16.0, 8.0, 11.0, 14.0, 17.0, 9.0, 12.0, 15.0, 18.0 ] );
* var WORK = new Float64Array( 6 );
*
* dtprfb( 'column-major', 'left', 'no-transpose', 'forward', 'columnwise', 4, 3, 2, 1, V, 4, T, 2, A, 2, B, 4, WORK, 2 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dtprfb = require( '@stdlib/lapack/base/dtprfb' );
*
* var V = new Float64Array( [ 1.0, 0.0, 0.0, 0.2, 0.3, 1.0, 0.0, 1.0 ] );
* var T = new Float64Array( [ 1.2, 0.0, -0.3, 0.8 ] );
* var A = new Float64Array( [ 1.0, 4.0, 2.0, 5.0, 3.0, 6.0 ] );
* var B = new Float64Array( [ 7.0, 10.0, 13.0, 16.0, 8.0, 11.0, 14.0, 17.0, 9.0, 12.0, 15.0, 18.0 ] );
* var WORK = new Float64Array( 6 );
*
* dtprfb.ndarray( 'left', 'no-transpose', 'forward', 'columnwise', 4, 3, 2, 1, V, 1, 4, 0, T, 1, 2, 0, A, 1, 2, 0, B, 1, 4, 0, WORK, 1, 2, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtprfb;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtprfb = main;
} else {
	dtprfb = tmp;
}


// EXPORTS //

module.exports = dtprfb;

// exports: { "ndarray": "dtprfb.ndarray" }
