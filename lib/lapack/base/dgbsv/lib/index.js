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
* Solve a banded system of linear equations.
*
* @module @stdlib/lapack/base/dgbsv
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dgbsv = require( '@stdlib/lapack/base/dgbsv' );
*
* var AB = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var IPIV = new Int32Array( 2 );
* var B = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* dgbsv( 'row-major', 2, 2, 2, 1, AB, 2, IPIV, 1, B, 2 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dgbsv = require( '@stdlib/lapack/base/dgbsv' );
*
* var AB = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var IPIV = new Int32Array( 2 );
* var B = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* dgbsv.ndarray( 2, 2, 2, 1, AB, 1, 2, 0, IPIV, 1, 0, B, 1, 2, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dgbsv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgbsv = main;
} else {
	dgbsv = tmp;
}


// EXPORTS //

module.exports = dgbsv;

// exports: { "ndarray": "dgbsv.ndarray" }
