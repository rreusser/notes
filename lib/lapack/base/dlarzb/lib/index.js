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
* Apply a real block reflector from an RZ factorization to a general matrix.
*
* @module @stdlib/lapack/base/dlarzb
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlarzb = require( '@stdlib/lapack/base/dlarzb' );
*
* var V = new Float64Array( [ 0.3, -0.4 ] );
* var T = new Float64Array( [ 0.8 ] );
* var C = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
* var WORK = new Float64Array( 3 );
*
* dlarzb( 'column-major', 'left', 'no-transpose', 'backward', 'rowwise', 2, 3, 1, 2, V, 1, T, 1, C, 2, WORK, 3 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlarzb = require( '@stdlib/lapack/base/dlarzb' );
*
* var V = new Float64Array( [ 0.3, -0.4 ] );
* var T = new Float64Array( [ 0.8 ] );
* var C = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
* var WORK = new Float64Array( 3 );
*
* dlarzb.ndarray( 'left', 'no-transpose', 'backward', 'rowwise', 2, 3, 1, 2, V, 1, 1, 0, T, 1, 1, 0, C, 1, 2, 0, WORK, 1, 3, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlarzb;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlarzb = main;
} else {
	dlarzb = tmp;
}


// EXPORTS //

module.exports = dlarzb;

// exports: { "ndarray": "dlarzb.ndarray" }
