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
* Compute a QR factorization with non-negative diagonal elements (unblocked).
*
* @module @stdlib/lapack/base/dgeqr2p
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dgeqr2p = require( '@stdlib/lapack/base/dgeqr2p' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var TAU = new Float64Array( [ 0.0, 0.0 ] );
* var WORK = new Float64Array( [ 0.0, 0.0 ] );
*
* dgeqr2p( 'column-major', 2, 2, A, 2, TAU, 1, WORK, 1 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dgeqr2p = require( '@stdlib/lapack/base/dgeqr2p' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var TAU = new Float64Array( [ 0.0, 0.0 ] );
* var WORK = new Float64Array( [ 0.0, 0.0 ] );
*
* dgeqr2p.ndarray( 2, 2, A, 1, 2, 0, TAU, 1, 0, WORK, 1, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dgeqr2p;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgeqr2p = main;
} else {
	dgeqr2p = tmp;
}


// EXPORTS //

module.exports = dgeqr2p;

// exports: { "ndarray": "dgeqr2p.ndarray" }
