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
* Solve a system of linear equations with a symmetric positive definite packed Cholesky-factored matrix.
*
* @module @stdlib/lapack/base/dpptrs
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dpptrs = require( '@stdlib/lapack/base/dpptrs' );
*
* var AP = new Float64Array( [ 2.0, 1.0, 2.0, 0.5, 1.25, 2.68095132369090194 ] );
* var B = new Float64Array( [ 1.0, 2.0, 3.0 ] );
*
* dpptrs( 'column-major', 'upper', 3, 1, AP, B, 3 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dpptrs = require( '@stdlib/lapack/base/dpptrs' );
*
* var AP = new Float64Array( [ 2.0, 1.0, 2.0, 0.5, 1.25, 2.68095132369090194 ] );
* var B = new Float64Array( [ 1.0, 2.0, 3.0 ] );
*
* dpptrs.ndarray( 'upper', 3, 1, AP, 1, 0, B, 1, 3, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dpptrs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dpptrs = main;
} else {
	dpptrs = tmp;
}


// EXPORTS //

module.exports = dpptrs;

// exports: { "ndarray": "dpptrs.ndarray" }
