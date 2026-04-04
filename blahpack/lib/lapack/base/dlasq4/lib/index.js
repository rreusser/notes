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
* Compute approximate singular value for dqds iteration.
*
* @module @stdlib/lapack/base/dlasq4
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlasq4 = require( '@stdlib/lapack/base/dlasq4' );
*
* var z = new Float64Array( [ 1.0, 2.0 ] );
*
* dlasq4( 2, 2, z, 1, 2, 2, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 2, 1.0 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlasq4 = require( '@stdlib/lapack/base/dlasq4' );
*
* var z = new Float64Array( [ 1.0, 2.0 ] );
*
* dlasq4.ndarray( 2, 2, z, 1, 0, 2, 2, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 2, 1.0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlasq4;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlasq4 = main;
} else {
	dlasq4 = tmp;
}


// EXPORTS //

module.exports = dlasq4;

// exports: { "ndarray": "dlasq4.ndarray" }
