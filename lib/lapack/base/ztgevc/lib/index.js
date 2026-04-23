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
* Compute eigenvectors of a pair of complex upper triangular matrices.
*
* @module @stdlib/lapack/base/ztgevc
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var ztgevc = require( '@stdlib/lapack/base/ztgevc' );
*
* var S = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var P = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var VL = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var VR = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var WORK = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
* var RWORK = new Float64Array( [ 1.0, 2.0 ] );
*
* ztgevc( 'row-major', 'left', 'all', 1.0, 1, 0, 2, S, 2, P, 2, VL, 2, VR, 2, 2, 2, WORK, 1, RWORK, 1 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var ztgevc = require( '@stdlib/lapack/base/ztgevc' );
*
* var SELECT = new Float64Array( [ 1.0, 2.0 ] );
* var S = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var P = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var VL = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var VR = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var WORK = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
* var RWORK = new Float64Array( [ 1.0, 2.0 ] );
*
* ztgevc.ndarray( 'left', 'all', SELECT, 1, 0, 2, S, 1, 2, 0, P, 1, 2, 0, VL, 1, 2, 0, VR, 1, 2, 0, 2, 2, WORK, 1, 0, RWORK, 1, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var ztgevc;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztgevc = main;
} else {
	ztgevc = tmp;
}


// EXPORTS //

module.exports = ztgevc;

// exports: { "ndarray": "ztgevc.ndarray" }
