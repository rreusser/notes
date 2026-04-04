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
* Solve a triangular system with scaling to prevent overflow, where the matrix is in packed storage.
*
* @module @stdlib/lapack/base/dlatps
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlatps = require( '@stdlib/lapack/base/dlatps' );
*
* var AP = new Float64Array( [ 2.0, 1.0, 3.0, 1.0, 2.0, 4.0 ] );
* var x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
* var scale = new Float64Array( 1 );
* var CNORM = new Float64Array( 3 );
*
* dlatps( 'upper', 'no-transpose', 'non-unit', 'no', 3, AP, x, 1, scale, CNORM, 1 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlatps = require( '@stdlib/lapack/base/dlatps' );
*
* var AP = new Float64Array( [ 2.0, 1.0, 3.0, 1.0, 2.0, 4.0 ] );
* var x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
* var scale = new Float64Array( 1 );
* var CNORM = new Float64Array( 3 );
*
* dlatps.ndarray( 'upper', 'no-transpose', 'non-unit', 'no', 3, AP, 1, 0, x, 1, 0, scale, CNORM, 1, 0 );
*/


// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dlatps.ndarray" }
