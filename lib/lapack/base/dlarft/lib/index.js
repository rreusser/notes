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
* Form the triangular factor T of a real block reflector.
*
* @module @stdlib/lapack/base/dlarft
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlarft = require( '@stdlib/lapack/base/dlarft' );
*
* var V = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var TAU = new Float64Array( [ 1.0, 2.0 ] );
* var T = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* dlarft( 'row-major', 'forward', 'column-wise', 2, 2, V, 2, TAU, 1, T, 2 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlarft = require( '@stdlib/lapack/base/dlarft' );
*
* var V = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var TAU = new Float64Array( [ 1.0, 2.0 ] );
* var T = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* dlarft.ndarray( 'forward', 'column-wise', 2, 2, V, 1, 2, 0, TAU, 1, 0, T, 1, 2, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlarft;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlarft = main;
} else {
	dlarft = tmp;
}


// EXPORTS //

module.exports = dlarft;

// exports: { "ndarray": "dlarft.ndarray" }
