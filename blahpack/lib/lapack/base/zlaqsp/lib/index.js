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
* Equilibrate a complex symmetric matrix in packed storage using scaling factors.
*
* @module @stdlib/lapack/base/zlaqsp
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zlaqsp = require( '@stdlib/lapack/base/zlaqsp' );
*
* // 3x3 complex symmetric matrix (upper packed):
* var AP = new Complex128Array( [ 4.0, 1.0, 1.0, 0.5, 9.0, 2.0, 0.5, 0.25, 2.0, 1.0, 16.0, 3.0 ] );
* var S = new Float64Array( [ 0.5, 0.5, 0.5 ] );
*
* var equed = zlaqsp( 'upper', 3, AP, S, 1, 0.01, 16.0 );
* // equed => 'yes'
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zlaqsp = require( '@stdlib/lapack/base/zlaqsp' );
*
* var AP = new Complex128Array( [ 4.0, 1.0, 1.0, 0.5, 9.0, 2.0, 0.5, 0.25, 2.0, 1.0, 16.0, 3.0 ] );
* var S = new Float64Array( [ 0.5, 0.5, 0.5 ] );
*
* var equed = zlaqsp.ndarray( 'upper', 3, AP, 1, 0, S, 1, 0, 0.01, 16.0 );
* // equed => 'yes'
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlaqsp;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlaqsp = main;
} else {
	zlaqsp = tmp;
}


// EXPORTS //

module.exports = zlaqsp;

// exports: { "ndarray": "zlaqsp.ndarray" }
