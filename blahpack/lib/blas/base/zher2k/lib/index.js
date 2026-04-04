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
* Perform Hermitian rank-2k update.
*
* @module @stdlib/blas/base/zher2k
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zher2k = require( '@stdlib/blas/base/zher2k' );
*
* var A = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var B = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var C = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* zher2k( 'row-major', 'upper', 'no-transpose', 2, 2, 1.0, A, 2, B, 2, 0.0, C, 2 );
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zher2k = require( '@stdlib/blas/base/zher2k' );
*
* var A = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var B = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var C = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* zher2k.ndarray( 'upper', 'no-transpose', 2, 2, 1.0, A, 1, 2, 0, B, 1, 2, 0, 0.0, C, 1, 2, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zher2k;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zher2k = main;
} else {
	zher2k = tmp;
}


// EXPORTS //

module.exports = zher2k;

// exports: { "ndarray": "zher2k.ndarray" }
