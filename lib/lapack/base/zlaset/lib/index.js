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
* Initialize a complex matrix to given values.
*
* @module @stdlib/lapack/base/zlaset
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zlaset = require( '@stdlib/lapack/base/zlaset' );
*
* var A = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* zlaset( 'row-major', 'upper', 2, 2, 1.0, 0.0, A, 2 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zlaset = require( '@stdlib/lapack/base/zlaset' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* zlaset.ndarray( 'upper', 2, 2, 1.0, 0.0, A, 1, 2, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlaset;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlaset = main;
} else {
	zlaset = tmp;
}


// EXPORTS //

module.exports = zlaset;

// exports: { "ndarray": "zlaset.ndarray" }
