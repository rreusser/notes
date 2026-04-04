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
* Equilibrate a symmetric band matrix using scaling factors.
*
* @module @stdlib/lapack/base/dlaqsb
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlaqsb = require( '@stdlib/lapack/base/dlaqsb' );
*
* // 4x4 symmetric band matrix (upper, KD=1, LDAB=2):
* var AB = new Float64Array( [ 0.0, 4.0, 1.0, 9.0, 2.0, 16.0, 3.0, 25.0 ] );
* var S = new Float64Array( [ 0.5, 0.25, 0.2, 0.1 ] );
*
* var equed = dlaqsb( 'upper', 4, 1, AB, 2, S, 1, 0.02, 25.0 );
* // equed => 'yes'
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlaqsb = require( '@stdlib/lapack/base/dlaqsb' );
*
* var AB = new Float64Array( [ 0.0, 4.0, 1.0, 9.0, 2.0, 16.0, 3.0, 25.0 ] );
* var S = new Float64Array( [ 0.5, 0.25, 0.2, 0.1 ] );
*
* var equed = dlaqsb.ndarray( 'upper', 4, 1, AB, 1, 2, 0, S, 1, 0, 0.02, 25.0 );
* // equed => 'yes'
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlaqsb;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaqsb = main;
} else {
	dlaqsb = tmp;
}


// EXPORTS //

module.exports = dlaqsb;

// exports: { "ndarray": "dlaqsb.ndarray" }
