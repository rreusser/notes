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
* Solve a triangular system with scaling to prevent overflow.
*
* @module @stdlib/lapack/base/dlatrs
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlatrs = require( '@stdlib/lapack/base/dlatrs' );
* 
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var x = new Float64Array( [ 1.0, 2.0 ] );
* var CNORM = new Float64Array( [ 1.0, 2.0 ] );
* 
* dlatrs( 'row-major', 'upper', 'no-transpose', 'non-unit', 'N', 2, A, 2, x, 1, 1.0, CNORM, 1 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlatrs = require( '@stdlib/lapack/base/dlatrs' );
* 
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var x = new Float64Array( [ 1.0, 2.0 ] );
* var CNORM = new Float64Array( [ 1.0, 2.0 ] );
* 
* dlatrs.ndarray( 'upper', 'no-transpose', 'non-unit', 'N', 2, A, 1, 2, 0, x, 1, 0, 1.0, CNORM, 1, 0 );
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dlatrs.ndarray" }
