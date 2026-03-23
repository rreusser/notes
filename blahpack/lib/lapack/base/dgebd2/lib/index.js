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
* Reduce a general matrix to bidiagonal form (unblocked).
*
* @module @stdlib/lapack/base/dgebd2
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dgebd2 = require( '@stdlib/lapack/base/dgebd2' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var d = new Float64Array( [ 1.0, 2.0 ] );
* var e = new Float64Array( [ 1.0, 2.0 ] );
* var TAUQ = new Float64Array( [ 1.0, 2.0 ] );
* var TAUP = new Float64Array( [ 1.0, 2.0 ] );
* var WORK = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
*
* dgebd2( 'row-major', 2, 2, A, 2, d, 1, e, 1, TAUQ, 1, TAUP, 1, WORK, 1 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dgebd2 = require( '@stdlib/lapack/base/dgebd2' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var d = new Float64Array( [ 1.0, 2.0 ] );
* var e = new Float64Array( [ 1.0, 2.0 ] );
* var TAUQ = new Float64Array( [ 1.0, 2.0 ] );
* var TAUP = new Float64Array( [ 1.0, 2.0 ] );
* var WORK = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
*
* dgebd2.ndarray( 2, 2, A, 1, 2, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0 );
*/


// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dgebd2.ndarray" }
