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
* Generate a complex Givens plane rotation.
*
* @module @stdlib/lapack/base/zlartg
*
* @example
* var zlartg = require( '@stdlib/lapack/base/zlartg' );
*
* zlartg( 1.0, 1.0, 1.0 );
*
* @example
* var zlartg = require( '@stdlib/lapack/base/zlartg' );
*
* zlartg.ndarray( 1.0, 1.0, 1.0, 1.0, 1.0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlartg;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlartg = main;
} else {
	zlartg = tmp;
}


// EXPORTS //

module.exports = zlartg;

// exports: { "ndarray": "zlartg.ndarray" }
