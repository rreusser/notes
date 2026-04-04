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
* Compute the eigendecomposition of a 2-by-2 Hermitian matrix.
*
* @module @stdlib/lapack/base/zlaev2
*
* @example
* var Complex128 = require( '@stdlib/complex/float64/ctor' );
* var zlaev2 = require( '@stdlib/lapack/base/zlaev2' );
*
* var a = new Complex128( 5.0, 0.0 );
* var b = new Complex128( 1.0, 2.0 );
* var c = new Complex128( 3.0, 0.0 );
* var out = zlaev2( a, b, c );
* // returns { rt1: ~6.449, rt2: ~1.551, cs1: ~-0.839, sn1r: ~-0.243, sn1i: ~0.487 }
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlaev2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlaev2 = main;
} else {
	zlaev2 = tmp;
}


// EXPORTS //

module.exports = zlaev2;

// exports: { "ndarray": "zlaev2.ndarray" }
