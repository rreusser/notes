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

/**
* Conjugate a complex vector in-place.
*
* @module @stdlib/lapack/base/zlacgv
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zlacgv = require( '@stdlib/lapack/base/zlacgv' );
*
* var x = new Complex128Array( [ 1.0, 2.0 ] );
*
* zlacgv( 2, x, 1 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zlacgv = require( '@stdlib/lapack/base/zlacgv' );
*
* var x = new Float64Array( [ 1.0, 2.0 ] );
*
* zlacgv.ndarray( 2, x, 1, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlacgv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlacgv = main;
} else {
	zlacgv = tmp;
}


// EXPORTS //

module.exports = zlacgv;

// exports: { "ndarray": "zlacgv.ndarray" }
