/**
* @license Apache-2.0
*
* Copyright (c) 2025 Ricky Reusser.
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
* Compute all eigenvalues and optionally eigenvectors of a complex Hermitian band matrix.
*
* @module @stdlib/lapack/base/zhbev
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zhbev = require( '@stdlib/lapack/base/zhbev' );
*
* // 4x4 Hermitian tridiagonal (KD=1), upper band storage, column-major:
* // Diagonal: 4, 5, 6, 7; off-diagonal: (1+i), (2-i), (3+i)
* var AB = new Complex128Array( [ 0.0, 0.0, 4.0, 0.0, 1.0, 1.0, 5.0, 0.0, 2.0, -1.0, 6.0, 0.0, 3.0, 1.0, 7.0, 0.0 ] );
* var W = new Float64Array( 4 );
* var Z = new Complex128Array( 16 );
* var WORK = new Complex128Array( 4 );
* var RWORK = new Float64Array( 10 );
*
* zhbev( 'column-major', 'compute-vectors', 'upper', 4, 1, AB, 2, W, 1, Z, 4, WORK, 1, RWORK, 1 );
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zhbev = require( '@stdlib/lapack/base/zhbev' );
*
* // 4x4 Hermitian tridiagonal (KD=1), upper band storage:
* var AB = new Complex128Array( [ 0.0, 0.0, 4.0, 0.0, 1.0, 1.0, 5.0, 0.0, 2.0, -1.0, 6.0, 0.0, 3.0, 1.0, 7.0, 0.0 ] );
* var W = new Float64Array( 4 );
* var Z = new Complex128Array( 16 );
* var WORK = new Complex128Array( 4 );
* var RWORK = new Float64Array( 10 );
*
* zhbev.ndarray( 'compute-vectors', 'upper', 4, 1, AB, 1, 2, 0, W, 1, 0, Z, 1, 4, 0, WORK, 1, 0, RWORK, 1, 0 );
*/


// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zhbev.ndarray" }
