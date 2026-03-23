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
* Multiply a general matrix by the orthogonal matrix from an LQ factorization (blocked).
*
* @module @stdlib/lapack/base/dormlq
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dormlq = require( '@stdlib/lapack/base/dormlq' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var TAU = new Float64Array( [ 1.0, 2.0 ] );
* var C = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var WORK = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
*
* dormlq( 'row-major', 'left', 'no-transpose', 2, 2, 2, A, 2, TAU, 1, C, 2, WORK, 1, 8 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dormlq = require( '@stdlib/lapack/base/dormlq' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var TAU = new Float64Array( [ 1.0, 2.0 ] );
* var C = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var WORK = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
*
* dormlq.ndarray( 'left', 'no-transpose', 2, 2, 2, A, 1, 2, 0, TAU, 1, 0, C, 1, 2, 0, WORK, 1, 0, 8 );
*/


// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dormlq.ndarray" }
