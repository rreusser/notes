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
* Compute all eigenvalues and optionally eigenvectors of a real symmetric band matrix.
*
* @module @stdlib/lapack/base/dsbev
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dsbev = require( '@stdlib/lapack/base/dsbev' );
*
* // 4x4 tridiagonal (KD=1), upper band storage, column-major:
* var AB = new Float64Array( [ 0.0, 4.0, 1.0, 5.0, 2.0, 6.0, 3.0, 7.0 ] );
* var W = new Float64Array( 4 );
* var Z = new Float64Array( 16 );
* var WORK = new Float64Array( 10 );
*
* dsbev( 'column-major', 'compute-vectors', 'upper', 4, 1, AB, 2, W, 1, Z, 4, WORK, 1 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dsbev = require( '@stdlib/lapack/base/dsbev' );
*
* // 4x4 tridiagonal (KD=1), upper band storage:
* var AB = new Float64Array( [ 0.0, 4.0, 1.0, 5.0, 2.0, 6.0, 3.0, 7.0 ] );
* var W = new Float64Array( 4 );
* var Z = new Float64Array( 16 );
* var WORK = new Float64Array( 10 );
*
* dsbev.ndarray( 'compute-vectors', 'upper', 4, 1, AB, 1, 2, 0, W, 1, 0, Z, 1, 4, 0, WORK, 1, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dsbev;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsbev = main;
} else {
	dsbev = tmp;
}


// EXPORTS //

module.exports = dsbev;

// exports: { "ndarray": "dsbev.ndarray" }
