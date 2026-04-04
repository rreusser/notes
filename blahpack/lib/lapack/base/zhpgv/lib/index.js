/**
* @license Apache-2.0
*
* Copyright (c) 2025 Ricky Reusser.
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
* Compute all eigenvalues and optionally eigenvectors of a complex Hermitian-definite generalized eigenproblem in packed storage.
*
* @module @stdlib/lapack/base/zhpgv
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zhpgv = require( '@stdlib/lapack/base/zhpgv' );
*
* var AP = new Complex128Array( [ 4, 0, 1, 1, 5, 0, 2, -1, 3, 0, 6, 0 ] );
* var BP = new Complex128Array( [ 2, 0, 0, 0, 2, 0, 0, 0, 0, 0, 2, 0 ] );
* var W = new Float64Array( 3 );
* var Z = new Complex128Array( 9 );
* var WORK = new Complex128Array( 10 );
* var RWORK = new Float64Array( 10 );
*
* var info = zhpgv( 'column-major', 1, 'compute-vectors', 'lower', 3, AP, BP, W, Z, 3, WORK, RWORK );
* // info => 0
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zhpgv = require( '@stdlib/lapack/base/zhpgv' );
*
* var AP = new Complex128Array( [ 4, 0, 1, 1, 5, 0, 2, -1, 3, 0, 6, 0 ] );
* var BP = new Complex128Array( [ 2, 0, 0, 0, 2, 0, 0, 0, 0, 0, 2, 0 ] );
* var W = new Float64Array( 3 );
* var Z = new Complex128Array( 9 );
* var WORK = new Complex128Array( 10 );
* var RWORK = new Float64Array( 10 );
*
* zhpgv.ndarray( 1, 'compute-vectors', 'lower', 3, AP, 1, 0, BP, 1, 0, W, 1, 0, Z, 1, 3, 0, WORK, 1, 0, RWORK, 1, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhpgv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhpgv = main;
} else {
	zhpgv = tmp;
}


// EXPORTS //

module.exports = zhpgv;

// exports: { "ndarray": "zhpgv.ndarray" }
