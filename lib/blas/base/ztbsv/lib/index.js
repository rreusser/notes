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
* Solve a complex triangular banded system of equations.
*
* @module @stdlib/blas/base/ztbsv
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var ztbsv = require( '@stdlib/blas/base/ztbsv' );
*
* var A = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var x = new Complex128Array( [ 1.0, 2.0 ] );
*
* ztbsv( 'row-major', 'upper', 'no-transpose', 'non-unit', 2, 2, A, 2, x, 1 );
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var ztbsv = require( '@stdlib/blas/base/ztbsv' );
*
* var A = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var x = new Complex128Array( [ 1.0, 2.0 ] );
*
* ztbsv.ndarray( 'upper', 'no-transpose', 'non-unit', 2, 2, A, 1, 2, 0, x, 1, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var ztbsv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztbsv = main;
} else {
	ztbsv = tmp;
}


// EXPORTS //

module.exports = ztbsv;

// exports: { "ndarray": "ztbsv.ndarray" }
