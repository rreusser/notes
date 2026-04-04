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
* Scale a complex double-precision vector by a complex constant.
*
* @module @stdlib/blas/base/zscal
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zscal = require( '@stdlib/blas/base/zscal' );
*
* var zx = new Complex128Array( [ 1.0, 2.0 ] );
*
* zscal( 2, 1.0, zx, 1 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zscal = require( '@stdlib/blas/base/zscal' );
*
* var x = new Float64Array( [ 1.0, 2.0 ] );
*
* zscal.ndarray( 2, 1.0, x, 1, 0, 2 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zscal;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zscal = main;
} else {
	zscal = tmp;
}


// EXPORTS //

module.exports = zscal;

// exports: { "ndarray": "zscal.ndarray" }
