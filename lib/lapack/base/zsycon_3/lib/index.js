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
* Estimate the reciprocal of the 1-norm condition number of a complex symmetric matrix using the factorization computed by `zsytrf_rk`.
*
* @module @stdlib/lapack/base/zsycon_3
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zsycon3 = require( '@stdlib/lapack/base/zsycon_3' );
*
* var A = new Complex128Array( [ 4.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.0, 0.0 ] );
* var e = new Complex128Array( 3 );
* var IPIV = new Int32Array( [ 0, 1, 2 ] );
* var work = new Complex128Array( 6 );
* var rcond = new Float64Array( 1 );
*
* zsycon3( 'column-major', 'upper', 3, A, 3, e, 1, IPIV, 1, 4.0, rcond, work, 1 );
* // rcond[ 0 ] => 1
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zsycon3 = require( '@stdlib/lapack/base/zsycon_3' );
*
* var A = new Complex128Array( [ 4.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.0, 0.0 ] );
* var e = new Complex128Array( 3 );
* var IPIV = new Int32Array( [ 0, 1, 2 ] );
* var work = new Complex128Array( 6 );
* var rcond = new Float64Array( 1 );
*
* zsycon3.ndarray( 'upper', 3, A, 1, 3, 0, e, 1, 0, IPIV, 1, 0, 4.0, rcond, work, 1, 0 );
* // rcond[ 0 ] => 1
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zsycon3;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsycon3 = main;
} else {
	zsycon3 = tmp;
}


// EXPORTS //

module.exports = zsycon3;

// exports: { "ndarray": "zsycon3.ndarray" }
