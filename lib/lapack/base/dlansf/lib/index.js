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
* Returns the value of the one-norm, Frobenius norm, infinity-norm, or the largest absolute value of any element of a real symmetric matrix stored in Rectangular Full Packed (RFP) format.
*
* @module @stdlib/lapack/base/dlansf
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlansf = require( '@stdlib/lapack/base/dlansf' );
*
* var A = new Float64Array( [ 2.0, 5.0, 4.0, 1.0, 3.0, 6.0 ] );
* var WORK = new Float64Array( 3 );
*
* var result = dlansf( 'max', 'no-transpose', 'upper', 3, A, WORK );
* // returns 6.0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlansf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlansf = main;
} else {
	dlansf = tmp;
}


// EXPORTS //

module.exports = dlansf;

// exports: { "ndarray": "dlansf.ndarray" }
