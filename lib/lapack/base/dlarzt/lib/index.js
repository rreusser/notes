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
* Form the triangular factor T of a block reflector `H = I - V*T*V^T`.
*
* @module @stdlib/lapack/base/dlarzt
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlarzt = require( '@stdlib/lapack/base/dlarzt' );
*
* var V = new Float64Array( [ 1.0, 0.5, -0.3, 0.7 ] );
* var TAU = new Float64Array( [ 0.8 ] );
* var T = new Float64Array( 1 );
*
* dlarzt( 'row-major', 'backward', 'rowwise', 4, 1, V, 4, TAU, 1, T, 1 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlarzt = require( '@stdlib/lapack/base/dlarzt' );
*
* var V = new Float64Array( [ 1.0, 0.5, -0.3, 0.7 ] );
* var TAU = new Float64Array( [ 0.8 ] );
* var T = new Float64Array( 1 );
*
* dlarzt.ndarray( 'backward', 'rowwise', 4, 1, V, 1, 1, 0, TAU, 1, 0, T, 1, 1, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlarzt;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlarzt = main;
} else {
	dlarzt = tmp;
}


// EXPORTS //

module.exports = dlarzt;

// exports: { "ndarray": "dlarzt.ndarray" }
