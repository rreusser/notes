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
* Generate a complex Householder reflector.
*
* @module @stdlib/lapack/base/zlarfg
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zlarfg = require( '@stdlib/lapack/base/zlarfg' );
*
* var alpha = new Complex128Array( [ 1.0, 2.0 ] );
* var x = new Complex128Array( [ 1.0, 2.0 ] );
* var tau = new Complex128Array( [ 1.0, 2.0 ] );
*
* zlarfg( 2, alpha, 0, x, 1, tau, 0 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zlarfg = require( '@stdlib/lapack/base/zlarfg' );
*
* var x = new Float64Array( [ 1.0, 2.0 ] );
*
* zlarfg.ndarray( 2, 1.0, x, 1, 0, 1.0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlarfg;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlarfg = main;
} else {
	zlarfg = tmp;
}


// EXPORTS //

module.exports = zlarfg;

// exports: { "ndarray": "zlarfg.ndarray" }
