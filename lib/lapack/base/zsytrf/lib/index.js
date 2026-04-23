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
* Compute complex symmetric indefinite factorization with Bunch-Kaufman pivoting (blocked).
*
* @module @stdlib/lapack/base/zsytrf
*
* @example
* var zsytrf = require( '@stdlib/lapack/base/zsytrf' );
*
* zsytrf(  );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zsytrf = require( '@stdlib/lapack/base/zsytrf' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var IPIV = new Int32Array( 2 );
* var WORK = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
*
* zsytrf.ndarray( 'upper', 2, A, 1, 2, 0, IPIV, 1, 0, WORK, 1, 0, 8 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zsytrf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsytrf = main;
} else {
	zsytrf = tmp;
}


// EXPORTS //

module.exports = zsytrf;

// exports: { "ndarray": "zsytrf.ndarray" }
