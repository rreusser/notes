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
* Computes all eigenvalues and optionally eigenvectors of a real symmetric positive definite tridiagonal matrix.
*
* @module @stdlib/lapack/base/dpteqr
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dpteqr = require( '@stdlib/lapack/base/dpteqr' );
*
* var d = new Float64Array( [ 4.0, 4.0, 4.0 ] );
* var e = new Float64Array( [ 1.0, 1.0 ] );
* var Z = new Float64Array( 9 );
* var WORK = new Float64Array( 12 );
*
* dpteqr( 'column-major', 'initialize', 3, d, 1, e, 1, Z, 3, WORK, 1 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dpteqr = require( '@stdlib/lapack/base/dpteqr' );
*
* var d = new Float64Array( [ 4.0, 4.0, 4.0 ] );
* var e = new Float64Array( [ 1.0, 1.0 ] );
* var Z = new Float64Array( 9 );
* var WORK = new Float64Array( 12 );
*
* dpteqr.ndarray( 'initialize', 3, d, 1, 0, e, 1, 0, Z, 1, 3, 0, WORK, 1, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dpteqr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dpteqr = main;
} else {
	dpteqr = tmp;
}


// EXPORTS //

module.exports = dpteqr;

// exports: { "ndarray": "dpteqr.ndarray" }
