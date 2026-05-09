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
* Reduces a real M-by-N (M <= N) upper trapezoidal matrix to upper triangular form via the RZ factorization (blocked driver).
*
* @module @stdlib/lapack/base/dtzrzf
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dtzrzf = require( '@stdlib/lapack/base/dtzrzf' );
*
* var A = new Float64Array( [ 4.0, 0.0, 0.0, 1.0, 5.0, 0.0, 2.0, 1.0, 6.0, 3.0, 2.0, 1.0, 1.0, 4.0, 2.0 ] );
* var TAU = new Float64Array( 3 );
* var WORK = new Float64Array( 96 );
*
* dtzrzf( 'column-major', 3, 5, A, 3, TAU, 1, WORK, 1 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dtzrzf = require( '@stdlib/lapack/base/dtzrzf' );
*
* var A = new Float64Array( [ 4.0, 0.0, 0.0, 1.0, 5.0, 0.0, 2.0, 1.0, 6.0, 3.0, 2.0, 1.0, 1.0, 4.0, 2.0 ] );
* var TAU = new Float64Array( 3 );
* var WORK = new Float64Array( 96 );
*
* dtzrzf.ndarray( 3, 5, A, 1, 3, 0, TAU, 1, 0, WORK, 1, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtzrzf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtzrzf = main;
} else {
	dtzrzf = tmp;
}


// EXPORTS //

module.exports = dtzrzf;

// exports: { "ndarray": "dtzrzf.ndarray" }
