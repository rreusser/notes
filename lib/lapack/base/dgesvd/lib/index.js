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
* Compute the singular value decomposition of a real matrix.
*
* @module @stdlib/lapack/base/dgesvd
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dgesvd = require( '@stdlib/lapack/base/dgesvd' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var s = new Float64Array( 2 );
* var U = new Float64Array( 4 );
* var VT = new Float64Array( 4 );
*
* var info = dgesvd( 'row-major', 'all', 'all', 2, 2, A, 2, s, 1, U, 2, VT, 2 );
* // returns 0
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dgesvd = require( '@stdlib/lapack/base/dgesvd' );
*
* var A = new Float64Array( [ 1.0, 3.0, 2.0, 4.0 ] );
* var s = new Float64Array( 2 );
* var U = new Float64Array( 4 );
* var VT = new Float64Array( 4 );
*
* var info = dgesvd.ndarray( 'all', 'all', 2, 2, A, 1, 2, 0, s, 1, 0, U, 1, 2, 0, VT, 1, 2, 0 );
* // returns 0
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dgesvd;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgesvd = main;
} else {
	dgesvd = tmp;
}


// EXPORTS //

module.exports = dgesvd;

// exports: { "ndarray": "dgesvd.ndarray" }
