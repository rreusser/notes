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
* Solve a triangular banded system of equations.
*
* @module @stdlib/lapack/base/dtbtrs
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dtbtrs = require( '@stdlib/lapack/base/dtbtrs' );
*
* var AB = new Float64Array( [ 0, 3, 1, 4, 2, 5 ] );
* var B = new Float64Array( [ 1, 2, 3 ] );
*
* var info = dtbtrs( 'column-major', 'upper', 'no-transpose', 'non-unit', 3, 1, 1, AB, 2, B, 3 );
* // info => 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtbtrs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtbtrs = main;
} else {
	dtbtrs = tmp;
}


// EXPORTS //

module.exports = dtbtrs;

// exports: { "ndarray": "dtbtrs.ndarray" }
