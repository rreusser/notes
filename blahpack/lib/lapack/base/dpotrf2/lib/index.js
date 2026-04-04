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
* Compute the Cholesky factorization of a real symmetric positive definite matrix using the recursive algorithm.
*
* @module @stdlib/lapack/base/dpotrf2
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dpotrf2 = require( '@stdlib/lapack/base/dpotrf2' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* dpotrf2( 'row-major', 'upper', 2, A, 2 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dpotrf2 = require( '@stdlib/lapack/base/dpotrf2' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* dpotrf2.ndarray( 'upper', 2, A, 1, 2, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dpotrf2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dpotrf2 = main;
} else {
	dpotrf2 = tmp;
}


// EXPORTS //

module.exports = dpotrf2;

// exports: { "ndarray": "dpotrf2.ndarray" }
