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
* Compute all eigenvalues and optionally eigenvectors of a real symmetric-definite banded generalized eigenproblem.
*
* @module @stdlib/lapack/base/dsbgv
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dsbgv = require( '@stdlib/lapack/base/dsbgv' );
*
* // 3x3 diagonal (KA=KB=0), column-major:
* var AB = new Float64Array( [ 5.0, 6.0, 7.0 ] );
* var BB = new Float64Array( [ 2.0, 3.0, 4.0 ] );
* var W = new Float64Array( 3 );
* var Z = new Float64Array( 9 );
* var WORK = new Float64Array( 9 );
*
* dsbgv( 'column-major', 'compute-vectors', 'upper', 3, 0, 0, AB, 1, BB, 1, W, 1, Z, 3, WORK, 1 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dsbgv = require( '@stdlib/lapack/base/dsbgv' );
*
* // 3x3 diagonal (KA=KB=0):
* var AB = new Float64Array( [ 5.0, 6.0, 7.0 ] );
* var BB = new Float64Array( [ 2.0, 3.0, 4.0 ] );
* var W = new Float64Array( 3 );
* var Z = new Float64Array( 9 );
* var WORK = new Float64Array( 9 );
*
* dsbgv.ndarray( 'compute-vectors', 'upper', 3, 0, 0, AB, 1, 1, 0, BB, 1, 1, 0, W, 1, 0, Z, 1, 3, 0, WORK, 1, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dsbgv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsbgv = main;
} else {
	dsbgv = tmp;
}


// EXPORTS //

module.exports = dsbgv;

// exports: { "ndarray": "dsbgv.ndarray" }
