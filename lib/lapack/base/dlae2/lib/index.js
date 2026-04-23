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
* Compute the eigenvalues of a 2-by-2 symmetric matrix.
*
* @module @stdlib/lapack/base/dlae2
*
* @example
* var dlae2 = require( '@stdlib/lapack/base/dlae2' );
*
* var out = dlae2( 1.0, 2.0, 1.0 );
* // returns { 'rt1': 3.0, 'rt2': -1.0 }
*
* @example
* var dlae2 = require( '@stdlib/lapack/base/dlae2' );
*
* var out = dlae2.ndarray( 1.0, 2.0, 1.0 );
* // returns { 'rt1': 3.0, 'rt2': -1.0 }
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlae2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlae2 = main;
} else {
	dlae2 = tmp;
}


// EXPORTS //

module.exports = dlae2;

// exports: { "ndarray": "dlae2.ndarray" }
