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
* Perform one of the matrix-vector operations y := alpha*A*x + beta*y or y := alpha*A**T*x + beta*y.
*
* @module @stdlib/blas/base/dgemv
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dgemv = require( '@stdlib/blas/base/dgemv' );
* 
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var x = new Float64Array( [ 1.0, 2.0 ] );
* var y = new Float64Array( [ 1.0, 2.0 ] );
* 
* dgemv( 'row-major', 'no-transpose', 2, 2, 1.0, A, 2, x, 1, 0.0, y, 1 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dgemv = require( '@stdlib/blas/base/dgemv' );
* 
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var x = new Float64Array( [ 1.0, 2.0 ] );
* var y = new Float64Array( [ 1.0, 2.0 ] );
* 
* dgemv.ndarray( 'no-transpose', 2, 2, 1.0, A, 1, 2, 0, x, 1, 0, 0.0, y, 1, 0 );
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dgemv.ndarray" }
