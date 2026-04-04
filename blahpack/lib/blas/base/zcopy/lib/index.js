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
* Copy a complex double-precision vector.
*
* @module @stdlib/blas/base/zcopy
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zcopy = require( '@stdlib/blas/base/zcopy' );
*
* var zx = new Complex128Array( [ 1.0, 2.0 ] );
* var zy = new Complex128Array( [ 1.0, 2.0 ] );
*
* zcopy( 2, zx, 1, zy, 1 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zcopy = require( '@stdlib/blas/base/zcopy' );
*
* var x = new Float64Array( [ 1.0, 2.0 ] );
* var y = new Float64Array( [ 1.0, 2.0 ] );
*
* zcopy.ndarray( 2, x, 1, 0, 2, y, 1, 0, 2 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zcopy;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zcopy = main;
} else {
	zcopy = tmp;
}


// EXPORTS //

module.exports = zcopy;

// exports: { "ndarray": "zcopy.ndarray" }
