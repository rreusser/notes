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

/**
* Form the triangular factor T of a block reflector.
*
* @module @stdlib/lapack/base/zlarft
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zlarft = require( '@stdlib/lapack/base/zlarft' );
*
* var V = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var TAU = new Complex128Array( [ 1.0, 2.0 ] );
* var T = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* zlarft( 'row-major', 'forward', 'column-wise', 2, 2, V, 2, TAU, 1, T, 2 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zlarft = require( '@stdlib/lapack/base/zlarft' );
*
* var V = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var TAU = new Float64Array( [ 1.0, 2.0 ] );
* var T = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* zlarft.ndarray( 'forward', 'column-wise', 2, 2, V, 1, 2, 0, TAU, 1, 0, T, 1, 2, 0 );
*/


// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zlarft.ndarray" }
