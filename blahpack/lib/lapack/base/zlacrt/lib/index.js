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
* Apply a plane rotation with complex cosine and sine to two complex vectors.
*
* @module @stdlib/lapack/base/zlacrt
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Complex128 = require( '@stdlib/complex/float64/ctor' );
* var zlacrt = require( '@stdlib/lapack/base/zlacrt' );
*
* var cx = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var cy = new Complex128Array( [ 5.0, 6.0, 7.0, 8.0 ] );
* var c = new Complex128( 0.6, 0.1 );
* var s = new Complex128( 0.8, 0.2 );
*
* zlacrt.ndarray( 2, cx, 1, 0, cy, 1, 0, c, s );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlacrt;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlacrt = main;
} else {
	zlacrt = tmp;
}


// EXPORTS //

module.exports = zlacrt;

// exports: { "ndarray": "zlacrt.ndarray" }
