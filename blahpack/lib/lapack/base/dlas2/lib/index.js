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
* Compute singular values of a 2-by-2 triangular matrix.
*
* @module @stdlib/lapack/base/dlas2
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlas2 = require( '@stdlib/lapack/base/dlas2' );
*
* var out = new Float64Array( [ 1.0, 2.0 ] );
*
* dlas2( 1.0, 1.0, 1.0, out );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlas2 = require( '@stdlib/lapack/base/dlas2' );
*
* var out = new Float64Array( [ 1.0, 2.0 ] );
*
* dlas2.ndarray( 1.0, 1.0, 1.0, out );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlas2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlas2 = main;
} else {
	dlas2 = tmp;
}


// EXPORTS //

module.exports = dlas2;

// exports: { "ndarray": "dlas2.ndarray" }
