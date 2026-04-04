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
* Perform the matrix-vector operation `y = alpha*A*x + beta*y` where `A` is a
* symmetric matrix supplied in packed form.
*
* @module @stdlib/blas/base/dspmv
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dspmv = require( '@stdlib/blas/base/dspmv' );
*
* var AP = new Float64Array( [ 1.0, 2.0, 3.0 ] );
* var x = new Float64Array( [ 1.0, 1.0 ] );
* var y = new Float64Array( [ 0.0, 0.0 ] );
*
* dspmv.ndarray( 'upper', 2, 1.0, AP, 1, 0, x, 1, 0, 0.0, y, 1, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dspmv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dspmv = main;
} else {
	dspmv = tmp;
}


// EXPORTS //

module.exports = dspmv;

// exports: { "ndarray": "dspmv.ndarray" }
