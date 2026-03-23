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
* Implement the single-shift QZ method for computing generalized eigenvalues of a complex matrix pair in Hessenberg-triangular form.
*
* @module @stdlib/lapack/base/zhgeqz
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zhgeqz = require( '@stdlib/lapack/base/zhgeqz' );
*
* var H = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var T = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var ALPHA = new Complex128Array( [ 1.0, 2.0 ] );
* var BETA = new Complex128Array( [ 1.0, 2.0 ] );
* var Q = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var Z = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var WORK = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
* var RWORK = new Float64Array( [ 1.0, 2.0 ] );
*
* zhgeqz( 'row-major', 'none', 'none', 'none', 2, 1, 2, H, 2, T, 2, ALPHA, 1, BETA, 1, Q, 2, Z, 2, WORK, 1, 8, RWORK, 1 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zhgeqz = require( '@stdlib/lapack/base/zhgeqz' );
*
* var H = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var T = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var ALPHA = new Float64Array( [ 1.0, 2.0 ] );
* var BETA = new Float64Array( [ 1.0, 2.0 ] );
* var Q = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var Z = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var WORK = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
* var RWORK = new Float64Array( [ 1.0, 2.0 ] );
*
* zhgeqz.ndarray( 'none', 'none', 'none', 2, 1, 2, H, 1, 2, 0, T, 1, 2, 0, ALPHA, 1, 0, BETA, 1, 0, Q, 1, 2, 0, Z, 1, 2, 0, WORK, 1, 0, 8, RWORK, 1, 0 );
*/


// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zhgeqz.ndarray" }
