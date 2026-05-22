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
* Compute the inverse of a real symmetric indefinite matrix using the factorization produced by `dsytrf`.
*
* @module @stdlib/lapack/base/dsytri2
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dsytri2 = require( '@stdlib/lapack/base/dsytri2' );
*
* var A = new Float64Array( [ 5.0 ] );
* var IPIV = new Int32Array( [ 0 ] );
*
* dsytri2( 'column-major', 'lower', 1, A, 1, IPIV );
* // A[0] ~ 0.2
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dsytri2 = require( '@stdlib/lapack/base/dsytri2' );
*
* var A = new Float64Array( [ 5.0 ] );
* var IPIV = new Int32Array( [ 0 ] );
* var WORK = new Float64Array( 1 );
*
* dsytri2.ndarray( 'lower', 1, A, 1, 1, 0, IPIV, 1, 0, WORK, 1, 0 );
* // A[0] ~ 0.2
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dsytri2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsytri2 = main;
} else {
	dsytri2 = tmp;
}


// EXPORTS //

module.exports = dsytri2;

// exports: { "ndarray": "dsytri2.ndarray" }
