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
* Computes the Bunch-Kaufman factorization of a complex Hermitian matrix in packed storage.
*
* @module @stdlib/lapack/base/zhptrf
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zhptrf = require( '@stdlib/lapack/base/zhptrf' );
*
* // 3x3 Hermitian positive definite matrix (lower packed):
* var AP = new Complex128Array( [ 4, 0, 1, -2, 3, 1, 5, 0, 2, -1, 7, 0 ] );
* var IPIV = new Int32Array( 3 );
*
* var info = zhptrf( 'lower', 3, AP, IPIV );
* // info => 0
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zhptrf = require( '@stdlib/lapack/base/zhptrf' );
*
* var AP = new Complex128Array( [ 4, 0, 1, -2, 3, 1, 5, 0, 2, -1, 7, 0 ] );
* var IPIV = new Int32Array( 3 );
*
* var info = zhptrf.ndarray( 'lower', 3, AP, 1, 0, IPIV, 1, 0 );
* // info => 0
*/


// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zhptrf.ndarray" }
