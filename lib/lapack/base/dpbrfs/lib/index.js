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
* LAPACK routine to improve the computed solution to a system `A * X = B` where `A` is symmetric positive definite band, and provide error bounds.
*
* @module @stdlib/lapack/base/dpbrfs
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dpbrfs = require( '@stdlib/lapack/base/dpbrfs' );
*
* var ab = new Float64Array( [ 0.0, 4.0, 1.0, 5.0, 1.0, 6.0 ] );
* var afb = new Float64Array( [ 0.0, 2.0, 0.5, 2.179449, 0.229416, 2.390457 ] );
* var b = new Float64Array( [ 1.0, 2.0, 3.0 ] );
* var x = new Float64Array( [ 0.181818, 0.272727, 0.454545 ] );
* var FERR = new Float64Array( 1 );
* var BERR = new Float64Array( 1 );
* var WORK = new Float64Array( 9 );
* var IWORK = new Int32Array( 3 );
*
* var info = dpbrfs.ndarray( 'upper', 3, 1, 1, ab, 1, 2, 0, afb, 1, 2, 0, b, 1, 3, 0, x, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
* // info => 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dpbrfs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dpbrfs = main;
} else {
	dpbrfs = tmp;
}


// EXPORTS //

module.exports = dpbrfs;

// exports: { "ndarray": "dpbrfs.ndarray" }
