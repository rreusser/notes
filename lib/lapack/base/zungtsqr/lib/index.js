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
* Generate an `M`-by-`N` complex matrix `Q` with orthonormal columns from a Tall-Skinny QR factorization (output of `zlatsqr`).
*
* @module @stdlib/lapack/base/zungtsqr
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zungtsqr = require( '@stdlib/lapack/base/zungtsqr' );
*
* // After running zlatsqr on an M-by-N panel to obtain V (in A) and T:
* var M = 4;
* var N = 2;
* var mb = 3;
* var nb = 2;
* var A = new Complex128Array( M * N ); // V from zlatsqr
* var T = new Complex128Array( nb * 2 * N ); // numblk*N columns
* var WORK = new Complex128Array( ( M + nb ) * N );
*
* zungtsqr( 'column-major', M, N, mb, nb, A, M, T, nb, WORK, 1 );
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zungtsqr = require( '@stdlib/lapack/base/zungtsqr' );
*
* var M = 4;
* var N = 2;
* var mb = 3;
* var nb = 2;
* var A = new Complex128Array( M * N );
* var T = new Complex128Array( nb * 2 * N );
* var WORK = new Complex128Array( ( M + nb ) * N );
*
* zungtsqr.ndarray( M, N, mb, nb, A, 1, M, 0, T, 1, nb, 0, WORK, 1, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zungtsqr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zungtsqr = main;
} else {
	zungtsqr = tmp;
}


// EXPORTS //

module.exports = zungtsqr;

// exports: { "ndarray": "zungtsqr.ndarray" }
