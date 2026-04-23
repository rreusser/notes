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
* Compute the inverse of a real symmetric indefinite matrix using the factorization produced by `dsytrf` (classic Bunch-Kaufman).
*
* @module @stdlib/lapack/base/dsytri2x
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dsytri2x = require( '@stdlib/lapack/base/dsytri2x' );
*
* var A = new Float64Array( [ 5.0 ] );
* var IPIV = new Int32Array( [ 0 ] );
* var work = new Float64Array( 20 );
*
* dsytri2x( 'column-major', 'lower', 1, A, 1, IPIV, 1, 0, work, 1, 2 );
* // A => <Float64Array>[ 0.2 ]
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dsytri2x = require( '@stdlib/lapack/base/dsytri2x' );
*
* var A = new Float64Array( [ 5.0 ] );
* var IPIV = new Int32Array( [ 0 ] );
* var work = new Float64Array( 20 );
*
* dsytri2x.ndarray( 'lower', 1, A, 1, 1, 0, IPIV, 1, 0, work, 1, 0, 2 );
* // A => <Float64Array>[ 0.2 ]
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dsytri2x;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsytri2x = main;
} else {
	dsytri2x = tmp;
}


// EXPORTS //

module.exports = dsytri2x;

// exports: { "ndarray": "dsytri2x.ndarray" }
