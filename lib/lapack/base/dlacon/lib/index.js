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
* Estimate the 1-norm of a square real matrix using reverse communication.
*
* @module @stdlib/lapack/base/dlacon
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dlacon = require( '@stdlib/lapack/base/dlacon' );
*
* var v = new Float64Array( 2 );
* var x = new Float64Array( 2 );
* var ISGN = new Int32Array( 2 );
* var EST = new Float64Array( 1 );
* var KASE = new Int32Array( 1 );
*
* dlacon( 2, v, 1, x, 1, ISGN, 1, EST, KASE );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dlacon = require( '@stdlib/lapack/base/dlacon' );
*
* var v = new Float64Array( 2 );
* var x = new Float64Array( 2 );
* var ISGN = new Int32Array( 2 );
* var EST = new Float64Array( 1 );
* var KASE = new Int32Array( 1 );
*
* dlacon.ndarray( 2, v, 1, 0, x, 1, 0, ISGN, 1, 0, EST, KASE );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlacon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlacon = main;
} else {
	dlacon = tmp;
}


// EXPORTS //

module.exports = dlacon;

// exports: { "ndarray": "dlacon.ndarray" }
