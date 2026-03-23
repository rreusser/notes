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
* Compute a partial factorization of a symmetric matrix using Bunch-Kaufman pivoting.
*
* @module @stdlib/lapack/base/dlasyf
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dlasyf = require( '@stdlib/lapack/base/dlasyf' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var IPIV = new Int32Array( 2 );
* var W = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* dlasyf( 'row-major', 'upper', 2, 2, A, 2, IPIV, 1, W, 2 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dlasyf = require( '@stdlib/lapack/base/dlasyf' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var IPIV = new Int32Array( 2 );
* var W = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* dlasyf.ndarray( 'upper', 2, 2, 2, A, 1, 2, 0, IPIV, 1, 0, W, 1, 2, 0 );
*/


// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dlasyf.ndarray" }
