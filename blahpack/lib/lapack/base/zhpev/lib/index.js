/**
* @license Apache-2.0
*
* Copyright (c) 2025 Ricky Reusser.
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
* Compute all eigenvalues and optionally eigenvectors of a complex Hermitian matrix in packed storage.
*
* @module @stdlib/lapack/base/zhpev
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zhpev = require( '@stdlib/lapack/base/zhpev' );
*
* // 2x2 Hermitian [[3,1-i],[1+i,5]] upper packed: [(3,0), (1,-1), (5,0)]
* var AP = new Complex128Array( [ 3.0, 0.0, 1.0, -1.0, 5.0, 0.0 ] );
* var w = new Float64Array( 2 );
* var Z = new Complex128Array( 4 );
* var WORK = new Complex128Array( 4 );
* var RWORK = new Float64Array( 6 );
*
* zhpev( 'column-major', 'compute', 'upper', 2, AP, w, Z, 2, WORK, RWORK );
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zhpev = require( '@stdlib/lapack/base/zhpev' );
*
* // 2x2 Hermitian [[3,1-i],[1+i,5]] upper packed: [(3,0), (1,-1), (5,0)]
* var AP = new Complex128Array( [ 3.0, 0.0, 1.0, -1.0, 5.0, 0.0 ] );
* var w = new Float64Array( 2 );
* var Z = new Complex128Array( 4 );
* var WORK = new Complex128Array( 4 );
* var RWORK = new Float64Array( 6 );
*
* zhpev.ndarray( 'compute', 'upper', 2, AP, 1, 0, w, 1, 0, Z, 1, 2, 0, WORK, 1, 0, RWORK, 1, 0 );
*/


// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhpev;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhpev = main;
} else {
	zhpev = tmp;
}


// EXPORTS //

module.exports = zhpev;

// exports: { "ndarray": "zhpev.ndarray" }
