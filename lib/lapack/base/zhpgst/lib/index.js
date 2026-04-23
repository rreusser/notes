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
* Reduce a complex Hermitian-definite generalized eigenproblem to standard form, using packed storage.
*
* @module @stdlib/lapack/base/zhpgst
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zhpgst = require( '@stdlib/lapack/base/zhpgst' );
*
* var AP = new Complex128Array( [ 12, 0, 3, 2, 9, 0, 1, -1, 2, 3, 7, 0 ] );
* var BP = new Complex128Array( [ 3, 0, 0, 0, 3, 0, 0, 0, 0, 0, 3, 0 ] );
*
* var info = zhpgst( 1, 'upper', 3, AP, BP );
* // info => 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhpgst;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhpgst = main;
} else {
	zhpgst = tmp;
}


// EXPORTS //

module.exports = zhpgst;

// exports: { "ndarray": "zhpgst.ndarray" }
