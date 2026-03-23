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
* Compute the SVD of a complex matrix.
*
* @module @stdlib/lapack/base/zgesvd
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zgesvd = require( '@stdlib/lapack/base/zgesvd' );
*
* var A = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var s = new Float64Array( [ 1.0, 2.0 ] );
* var U = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var VT = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var WORK = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
* var RWORK = new Float64Array( [ 1.0, 2.0 ] );
*
* zgesvd( 'row-major', 'none', 'none', 2, 2, A, 2, s, 1, U, 2, VT, 2, WORK, 1, 8, RWORK, 1 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zgesvd = require( '@stdlib/lapack/base/zgesvd' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var s = new Float64Array( [ 1.0, 2.0 ] );
* var U = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var VT = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var WORK = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
* var RWORK = new Float64Array( [ 1.0, 2.0 ] );
*
* zgesvd.ndarray( 'none', 'none', 2, 2, A, 1, 2, 0, s, 1, 0, U, 1, 2, 0, VT, 1, 2, 0, WORK, 1, 0, 8, RWORK, 1, 0 );
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zgesvd.ndarray" }
