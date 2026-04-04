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
* LAPACK routine to improve the computed solution to a complex system `A * X = B` where `A` is Hermitian positive definite band, and provide error bounds.
*
* @module @stdlib/lapack/base/zpbrfs
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zpbrfs = require( '@stdlib/lapack/base/zpbrfs' );
*
* var AB = new Complex128Array( [ 0.0, 0.0, 4.0, 0.0, 1.0, 1.0, 5.0, 0.0, 2.0, -1.0, 6.0, 0.0 ] );
* var AFB = new Complex128Array( [ 0.0, 0.0, 2.0, 0.0, 0.5, 0.5, 2.12132, 0.0, 0.942809, -0.471405, 2.21108, 0.0 ] );
* var B = new Complex128Array( [ 4.0, 2.0, 10.0, 2.0, 13.0, 3.0 ] );
* var X = new Complex128Array( [ 1.0, 0.0, 1.0, 1.0, 2.0, 0.0 ] );
* var FERR = new Float64Array( 1 );
* var BERR = new Float64Array( 1 );
* var WORK = new Complex128Array( 6 );
* var RWORK = new Float64Array( 3 );
*
* var info = zpbrfs.ndarray( 'upper', 3, 1, 1, AB, 1, 2, 0, AFB, 1, 2, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
* // info => 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zpbrfs.ndarray" }
