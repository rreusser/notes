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
* Solve a system of linear equations with a complex Hermitian positive definite packed Cholesky-factored matrix.
*
* @module @stdlib/lapack/base/zpptrs
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zpptrs = require( '@stdlib/lapack/base/zpptrs' );
*
* var AP = new Complex128Array( [ 3.162, 0.0, 0.949, -0.316, 2.646, 0.0, 0.316, 0.632, 0.718, -0.643, 2.138, 0.0 ] );
* var B = new Complex128Array( [ 1.0, 1.0, 2.0, -1.0, 3.0, 0.5 ] );
*
* zpptrs( 'column-major', 'upper', 3, 1, AP, B, 3 );
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zpptrs = require( '@stdlib/lapack/base/zpptrs' );
*
* var AP = new Complex128Array( [ 3.162, 0.0, 0.949, -0.316, 2.646, 0.0, 0.316, 0.632, 0.718, -0.643, 2.138, 0.0 ] );
* var B = new Complex128Array( [ 1.0, 1.0, 2.0, -1.0, 3.0, 0.5 ] );
*
* zpptrs.ndarray( 'upper', 3, 1, AP, 1, 0, B, 1, 3, 0 );
*/


// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zpptrs.ndarray" }
