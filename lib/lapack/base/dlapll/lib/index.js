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
* Measure the linear dependence of two vectors.
*
* @module @stdlib/lapack/base/dlapll
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlapll = require( '@stdlib/lapack/base/dlapll' );
*
* var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var y = new Float64Array( [ 2.0, 4.0, 6.0, 8.0 ] );
* var ssmin = new Float64Array( 1 );
*
* dlapll( 4, x, 1, y, 1, ssmin );
* // ssmin[ 0 ] ~ 0.0
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlapll = require( '@stdlib/lapack/base/dlapll' );
*
* var x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
* var y = new Float64Array( [ 4.0, 5.0, 6.0 ] );
* var ssmin = new Float64Array( 1 );
*
* dlapll.ndarray( 3, x, 1, 0, y, 1, 0, ssmin );
* // ssmin[ 0 ] ~ 0.773
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlapll;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlapll = main;
} else {
	dlapll = tmp;
}


// EXPORTS //

module.exports = dlapll;

// exports: { "ndarray": "dlapll.ndarray" }
