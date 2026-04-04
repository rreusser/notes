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
* Compute the inverse of a real triangular matrix in packed storage.
*
* @module @stdlib/lapack/base/dtptri
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dtptri = require( '@stdlib/lapack/base/dtptri' );
*
* // 3x3 upper triangular: A = [2 1 3; 0 4 5; 0 0 6]
* var AP = new Float64Array( [ 2.0, 1.0, 4.0, 3.0, 5.0, 6.0 ] );
*
* var info = dtptri( 'upper', 'non-unit', 3, AP );
* // info => 0
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dtptri = require( '@stdlib/lapack/base/dtptri' );
*
* // 3x3 upper triangular: A = [2 1 3; 0 4 5; 0 0 6]
* var AP = new Float64Array( [ 2.0, 1.0, 4.0, 3.0, 5.0, 6.0 ] );
*
* var info = dtptri.ndarray( 'upper', 'non-unit', 3, AP, 1, 0 );
* // info => 0
*/


// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dtptri.ndarray" }
