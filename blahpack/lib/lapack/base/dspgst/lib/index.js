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
* Reduce a real symmetric-definite generalized eigenproblem to standard form, using packed storage.
*
* @module @stdlib/lapack/base/dspgst
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dspgst = require( '@stdlib/lapack/base/dspgst' );
*
* var AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
* var BP = new Float64Array( [ 2.0, 0.0, 2.0, 0.0, 0.0, 1.5 ] );
*
* var info = dspgst( 1, 'upper', 3, AP, BP );
* // info => 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dspgst;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dspgst = main;
} else {
	dspgst = tmp;
}


// EXPORTS //

module.exports = dspgst;

// exports: { "ndarray": "dspgst.ndarray" }
