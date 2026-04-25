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
* Compute all eigenvalues and optionally eigenvectors of a real symmetric matrix in packed storage.
*
* @module @stdlib/lapack/base/dspev
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dspev = require( '@stdlib/lapack/base/dspev' );
*
* // 2x2 symmetric [[1,2],[2,3]] upper packed: [1, 2, 3]
* var AP = new Float64Array( [ 1.0, 2.0, 3.0 ] );
* var w = new Float64Array( 2 );
* var Z = new Float64Array( 4 );
* var WORK = new Float64Array( 6 );
*
* dspev( 'column-major', 'compute', 'upper', 2, AP, w, Z, 2, WORK );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dspev = require( '@stdlib/lapack/base/dspev' );
*
* // 2x2 symmetric [[1,2],[2,3]] upper packed: [1, 2, 3]
* var AP = new Float64Array( [ 1.0, 2.0, 3.0 ] );
* var w = new Float64Array( 2 );
* var Z = new Float64Array( 4 );
* var WORK = new Float64Array( 6 );
*
* dspev.ndarray( 'compute', 'upper', 2, AP, 1, 0, w, 1, 0, Z, 1, 2, 0, WORK, 1, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dspev;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dspev = main;
} else {
	dspev = tmp;
}


// EXPORTS //

module.exports = dspev;

// exports: { "ndarray": "dspev.ndarray" }
