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
* Applies an elementary reflector, or Householder matrix, H, to an N-by-N symmetric matrix C, from both sides.
*
* @module @stdlib/lapack/base/dlarfy
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlarfy = require( '@stdlib/lapack/base/dlarfy' );
*
* var v = new Float64Array( [ 1.0, 0.5 ] );
* var C = new Float64Array( [ 4.0, 1.0, 1.0, 5.0 ] );
* var WORK = new Float64Array( 2 );
*
* dlarfy( 'column-major', 'upper', 2, v, 1, 1.0, C, 2, WORK, 1 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlarfy = require( '@stdlib/lapack/base/dlarfy' );
*
* var v = new Float64Array( [ 1.0, 0.5 ] );
* var C = new Float64Array( [ 4.0, 1.0, 1.0, 5.0 ] );
* var WORK = new Float64Array( 2 );
*
* dlarfy.ndarray( 'upper', 2, v, 1, 0, 1.0, C, 1, 2, 0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlarfy;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlarfy = main;
} else {
	dlarfy = tmp;
}


// EXPORTS //

module.exports = dlarfy;

// exports: { "ndarray": "dlarfy.ndarray" }
