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
* Bounded Bunch-Kaufman (rook) factorization of a complex symmetric indefinite matrix.
*
* @module @stdlib/lapack/base/zsytf2_rook
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zsytf2Rook = require( '@stdlib/lapack/base/zsytf2_rook' );
*
* var A = new Complex128Array( [ 4.0, 1.0, 1.0, 2.0, 1.0, 2.0, 5.0, -1.0 ] );
* var IPIV = new Int32Array( 2 );
*
* zsytf2Rook( 'column-major', 'upper', 2, A, 2, IPIV, 1, 0 );
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zsytf2Rook = require( '@stdlib/lapack/base/zsytf2_rook' );
*
* var A = new Complex128Array( [ 4.0, 1.0, 1.0, 2.0, 1.0, 2.0, 5.0, -1.0 ] );
* var IPIV = new Int32Array( 2 );
*
* zsytf2Rook.ndarray( 'upper', 2, A, 1, 2, 0, IPIV, 1, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zsytf2Rook;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsytf2Rook = main;
} else {
	zsytf2Rook = tmp;
}


// EXPORTS //

module.exports = zsytf2Rook;

// exports: { "ndarray": "zsytf2_rook.ndarray" }
