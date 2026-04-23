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
* Generates a real orthogonal matrix Q from the elementary reflectors returned by dsptrd.
*
* @module @stdlib/lapack/base/dopgtr
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dopgtr = require( '@stdlib/lapack/base/dopgtr' );
*
* var AP = new Float64Array( [ 5.3, -0.1, 1.7, -0.72, 3.16, 4.0 ] );
* var TAU = new Float64Array( [ 0.0, 1.316 ] );
* var Q = new Float64Array( 9 );
* var WORK = new Float64Array( 3 );
*
* var info = dopgtr.ndarray( 'upper', 3, AP, 1, 0, TAU, 1, 0, Q, 1, 3, 0, WORK, 1, 0 );
* // returns 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dopgtr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dopgtr = main;
} else {
	dopgtr = tmp;
}


// EXPORTS //

module.exports = dopgtr;

// exports: { "ndarray": "dopgtr.ndarray" }
