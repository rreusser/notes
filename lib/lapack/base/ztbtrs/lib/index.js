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
* Solve a triangular banded system of equations with a complex triangular band matrix.
*
* @module @stdlib/lapack/base/ztbtrs
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var ztbtrs = require( '@stdlib/lapack/base/ztbtrs' );
*
* var AB = new Complex128Array( [ 0, 0, 3, 0, 1, 1, 4, 0, 2, 0, 5, 0 ] );
* var B = new Complex128Array( [ 1, 0, 2, 0, 3, 0 ] );
*
* var info = ztbtrs( 'column-major', 'upper', 'no-transpose', 'non-unit', 3, 1, 1, AB, 2, B, 3 );
* // info => 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var ztbtrs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztbtrs = main;
} else {
	ztbtrs = tmp;
}


// EXPORTS //

module.exports = ztbtrs;

// exports: { "ndarray": "ztbtrs.ndarray" }
