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
* Compute a column-blocked QR factorization of a complex M-by-N matrix using TSQR followed by Householder reconstruction.
*
* @module @stdlib/lapack/base/zgetsqrhrt
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zgetsqrhrt = require( '@stdlib/lapack/base/zgetsqrhrt' );
*
* var A = new Complex128Array( 16 );
* var T = new Complex128Array( 4 );
*
* zgetsqrhrt( 'column-major', 4, 2, 4, 2, 2, A, 4, T, 2 );
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zgetsqrhrt = require( '@stdlib/lapack/base/zgetsqrhrt' );
*
* var A = new Complex128Array( 16 );
* var T = new Complex128Array( 4 );
*
* zgetsqrhrt.ndarray( 4, 2, 4, 2, 2, A, 1, 4, 0, T, 1, 2, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgetsqrhrt;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgetsqrhrt = main;
} else {
	zgetsqrhrt = tmp;
}


// EXPORTS //

module.exports = zgetsqrhrt;

// exports: { "ndarray": "zgetsqrhrt.ndarray" }
