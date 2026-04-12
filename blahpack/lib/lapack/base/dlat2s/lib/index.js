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
* Converts a double-precision triangular matrix to a single-precision triangular matrix.
*
* @module @stdlib/lapack/base/dlat2s
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Float32Array = require( '@stdlib/array/float32' );
* var dlat2s = require( '@stdlib/lapack/base/dlat2s' );
*
* var A = new Float64Array( [ 1.0, 0.0, 2.0, 3.0 ] );
* var SA = new Float32Array( 4 );
*
* var info = dlat2s( 'column-major', 'upper', 2, A, 2, SA, 2 );
* // returns 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlat2s;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlat2s = main;
} else {
	dlat2s = tmp;
}


// EXPORTS //

module.exports = dlat2s;

// exports: { "ndarray": "dlat2s.ndarray" }
