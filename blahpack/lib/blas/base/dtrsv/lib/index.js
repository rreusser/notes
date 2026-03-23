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
* Solve a triangular system of equations with a single right-hand side.
*
* @module @stdlib/blas/base/dtrsv
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dtrsv = require( '@stdlib/blas/base/dtrsv' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var x = new Float64Array( [ 1.0, 2.0 ] );
*
* dtrsv( 'row-major', 'upper', 'no-transpose', 'non-unit', 2, A, 2, x, 1 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dtrsv = require( '@stdlib/blas/base/dtrsv' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var x = new Float64Array( [ 1.0, 2.0 ] );
*
* dtrsv.ndarray( 'upper', 'no-transpose', 'non-unit', 2, A, 1, 2, 0, x, 1, 0 );
*/


// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dtrsv.ndarray" }
