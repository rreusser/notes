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
* Compute the value of the one norm, Frobenius norm, infinity norm, or largest absolute value of a matrix.
*
* @module @stdlib/lapack/base/dlange
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlange = require( '@stdlib/lapack/base/dlange' );
* 
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var WORK = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
* 
* dlange( 'row-major', 'one-norm', 2, 2, A, 2, WORK, 1 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlange = require( '@stdlib/lapack/base/dlange' );
* 
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var WORK = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
* 
* dlange.ndarray( 'one-norm', 2, 2, A, 1, 2, 0, WORK, 1, 0 );
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dlange.ndarray" }
