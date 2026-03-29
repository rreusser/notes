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
* Solve a triangular system of equations with a triangular matrix stored in packed format.
*
* @module @stdlib/lapack/base/dtptrs
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dtptrs = require( '@stdlib/lapack/base/dtptrs' );
*
* var AP = new Float64Array( [ 2.0, 1.0, 4.0, 3.0, 5.0, 6.0 ] );
* var B = new Float64Array( [ 1.0, 2.0, 3.0 ] );
*
* dtptrs( 'column-major', 'upper', 'no-transpose', 'non-unit', 3, 1, AP, B, 3 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dtptrs = require( '@stdlib/lapack/base/dtptrs' );
*
* var AP = new Float64Array( [ 2.0, 1.0, 4.0, 3.0, 5.0, 6.0 ] );
* var B = new Float64Array( [ 1.0, 2.0, 3.0 ] );
*
* dtptrs.ndarray( 'upper', 'no-transpose', 'non-unit', 3, 1, AP, 1, 0, B, 1, 3, 0 );
*/


// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dtptrs.ndarray" }
