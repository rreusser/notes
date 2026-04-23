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
* Compute eigenvalues and optionally eigenvectors of a complex Hermitian matrix.
*
* @module @stdlib/lapack/base/zheev
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zheev = require( '@stdlib/lapack/base/zheev' );
*
* var A = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var w = new Float64Array( [ 1.0, 2.0 ] );
* var WORK = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
* var RWORK = new Float64Array( [ 1.0, 2.0 ] );
*
* zheev( 'row-major', 'none', 'upper', 2, A, 2, w, 1, WORK, 1, 8, RWORK, 1 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zheev = require( '@stdlib/lapack/base/zheev' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var w = new Float64Array( [ 1.0, 2.0 ] );
* var WORK = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
* var RWORK = new Float64Array( [ 1.0, 2.0 ] );
*
* zheev.ndarray( 'none', 'upper', 2, A, 1, 2, 0, w, 1, 0, WORK, 1, 0, 8, RWORK, 1, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zheev;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zheev = main;
} else {
	zheev = tmp;
}


// EXPORTS //

module.exports = zheev;

// exports: { "ndarray": "zheev.ndarray" }
