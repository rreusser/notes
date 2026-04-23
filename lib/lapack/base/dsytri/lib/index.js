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
* Computes the inverse of a real symmetric matrix using the factorization computed by dsytrf.
*
* @module @stdlib/lapack/base/dsytri
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dsytri = require( '@stdlib/lapack/base/dsytri' );
*
* var A = new Float64Array( [ 2.0, 0.5, 0.0, 1.5 ] );
* var IPIV = new Int32Array( [ 1, 1 ] );
* var WORK = new Float64Array( 2 );
*
* dsytri( 'column-major', 'lower', 2, A, 2, IPIV, 1, WORK );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dsytri = require( '@stdlib/lapack/base/dsytri' );
*
* var A = new Float64Array( [ 2.0, 0.5, 0.0, 1.5 ] );
* var IPIV = new Int32Array( [ 1, 1 ] );
* var WORK = new Float64Array( 2 );
*
* dsytri.ndarray( 'lower', 2, A, 1, 2, 0, IPIV, 1, 0, WORK, 1, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dsytri;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsytri = main;
} else {
	dsytri = tmp;
}


// EXPORTS //

module.exports = dsytri;

// exports: { "ndarray": "dsytri.ndarray" }
