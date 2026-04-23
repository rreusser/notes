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
* Performs a series of row interchanges (complex double-precision).
*
* @module @stdlib/lapack/base/zlaswp
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zlaswp = require( '@stdlib/lapack/base/zlaswp' );
*
* var A = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var IPIV = new Int32Array( 2 );
*
* zlaswp( 'row-major', 2, A, 2, 0, 1, IPIV, 1, 2 );
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zlaswp = require( '@stdlib/lapack/base/zlaswp' );
*
* var A = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var IPIV = new Int32Array( 2 );
*
* zlaswp.ndarray( 2, A, 1, 2, 0, 0, 1, IPIV, 1, 0, 2 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlaswp;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlaswp = main;
} else {
	zlaswp = tmp;
}


// EXPORTS //

module.exports = zlaswp;

// exports: { "ndarray": "zlaswp.ndarray" }
