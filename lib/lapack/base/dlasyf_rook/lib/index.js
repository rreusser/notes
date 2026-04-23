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
* Compute a partial factorization of a real symmetric indefinite matrix using the bounded Bunch-Kaufman ("rook") diagonal pivoting method.
*
* @module @stdlib/lapack/base/dlasyf-rook
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dlasyfRook = require( '@stdlib/lapack/base/dlasyf-rook' );
*
* var A = new Float64Array( [ 4.0, 1.0, 1.0, 3.0 ] );
* var IPIV = new Int32Array( 2 );
* var W = new Float64Array( 4 );
*
* var out = dlasyfRook( 'column-major', 'lower', 2, 2, A, 2, IPIV, W, 2 );
* // returns { 'info': 0, 'kb': 2 }
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dlasyfRook = require( '@stdlib/lapack/base/dlasyf-rook' );
*
* var A = new Float64Array( [ 4.0, 1.0, 1.0, 3.0 ] );
* var IPIV = new Int32Array( 2 );
* var W = new Float64Array( 4 );
*
* var out = dlasyfRook.ndarray( 'lower', 2, 2, A, 1, 2, 0, IPIV, 1, 0, W, 1, 2, 0 );
* // returns { 'info': 0, 'kb': 2 }
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlasyfRook;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlasyfRook = main;
} else {
	dlasyfRook = tmp;
}


// EXPORTS //

module.exports = dlasyfRook;

// exports: { "ndarray": "dlasyfRook.ndarray" }
