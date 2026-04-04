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
* Perform one of the matrix-matrix operations B := alpha_op(A)_B or B := alpha_B_op(A) where A is a triangular matrix.
*
* @module @stdlib/blas/base/dtrmm
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dtrmm = require( '@stdlib/blas/base/dtrmm' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var B = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* dtrmm( 'row-major', 'left', 'upper', 'no-transpose', 'non-unit', 2, 2, 1.0, A, 2, B, 2 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dtrmm = require( '@stdlib/blas/base/dtrmm' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var B = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* dtrmm.ndarray( 'left', 'upper', 'no-transpose', 'non-unit', 2, 2, 1.0, A, 1, 2, 0, B, 1, 2, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtrmm;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtrmm = main;
} else {
	dtrmm = tmp;
}


// EXPORTS //

module.exports = dtrmm;

// exports: { "ndarray": "dtrmm.ndarray" }
