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
* Solve a banded symmetric positive definite system using Cholesky factorization.
*
* @module @stdlib/lapack/base/dpbtrs
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dpbtrs = require( '@stdlib/lapack/base/dpbtrs' );
*
* var AB = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var B = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* dpbtrs( 'row-major', 'upper', 2, 2, 1, AB, 2, B, 2 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dpbtrs = require( '@stdlib/lapack/base/dpbtrs' );
*
* var AB = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var B = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* dpbtrs.ndarray( 'upper', 2, 2, 1, AB, 1, 2, 0, B, 1, 2, 0 );
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dpbtrs.ndarray" }
