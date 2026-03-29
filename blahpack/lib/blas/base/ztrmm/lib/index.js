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
* Perform one of the matrix-matrix operations B := alpha_op(A)_B or B := alpha_B_op(A).
*
* @module @stdlib/blas/base/ztrmm
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var ztrmm = require( '@stdlib/blas/base/ztrmm' );
*
* var A = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var B = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* ztrmm( 'row-major', 'left', 'upper', 'no-transpose', 'non-unit', 2, 2, 1.0, A, 2, B, 2 );
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var ztrmm = require( '@stdlib/blas/base/ztrmm' );
*
* var A = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var B = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* ztrmm.ndarray( 'left', 'upper', 'no-transpose', 'non-unit', 2, 2, 1.0, A, 1, 2, 0, B, 1, 2, 0 );
*/


// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "ztrmm.ndarray" }
