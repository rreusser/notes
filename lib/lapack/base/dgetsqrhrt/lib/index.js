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
* Compute a column-blocked QR factorization of a real `M`-by-`N` matrix `A` (with `M >= N`) via TSQR followed by Householder reconstruction.
*
* @module @stdlib/lapack/base/dgetsqrhrt
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dgetsqrhrt = require( '@stdlib/lapack/base/dgetsqrhrt' );
*
* var A = new Float64Array( [ 2.0, 1.0, 3.0, 1.0, 1.0, 4.0, 2.0, 3.0, 3.0, 2.0, 5.0, 1.0 ] );
* var T = new Float64Array( 6 );
* var WORK = new Float64Array( 64 );
*
* dgetsqrhrt( 'column-major', 4, 3, 4, 2, 2, A, 4, T, 2, WORK );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dgetsqrhrt = require( '@stdlib/lapack/base/dgetsqrhrt' );
*
* var A = new Float64Array( [ 2.0, 1.0, 3.0, 1.0, 1.0, 4.0, 2.0, 3.0, 3.0, 2.0, 5.0, 1.0 ] );
* var T = new Float64Array( 6 );
* var WORK = new Float64Array( 64 );
*
* dgetsqrhrt.ndarray( 4, 3, 4, 2, 2, A, 1, 4, 0, T, 1, 2, 0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dgetsqrhrt;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgetsqrhrt = main;
} else {
	dgetsqrhrt = tmp;
}


// EXPORTS //

module.exports = dgetsqrhrt;

// exports: { "ndarray": "dgetsqrhrt.ndarray" }
