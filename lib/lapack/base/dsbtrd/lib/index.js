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
* Reduce a real symmetric band matrix to symmetric tridiagonal form.
*
* @module @stdlib/lapack/base/dsbtrd
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dsbtrd = require( '@stdlib/lapack/base/dsbtrd' );
*
* // 4x4 symmetric tridiagonal (KD=1), upper band storage (LDAB=2):
* var AB = new Float64Array( [ 0.0, 4.0, 1.0, 5.0, 2.0, 6.0, 3.0, 7.0 ] );
* var d = new Float64Array( 4 );
* var e = new Float64Array( 3 );
* var Q = new Float64Array( 1 );
* var WORK = new Float64Array( 4 );
*
* dsbtrd( 'column-major', 'none', 'upper', 4, 1, AB, 2, d, e, Q, 1, WORK );
* // d => <Float64Array>[ 4.0, 5.0, 6.0, 7.0 ]
* // e => <Float64Array>[ 1.0, 2.0, 3.0 ]
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dsbtrd = require( '@stdlib/lapack/base/dsbtrd' );
*
* // 4x4 symmetric tridiagonal (KD=1), upper band storage:
* var AB = new Float64Array( [ 0.0, 4.0, 1.0, 5.0, 2.0, 6.0, 3.0, 7.0 ] );
* var d = new Float64Array( 4 );
* var e = new Float64Array( 3 );
* var Q = new Float64Array( 1 );
* var WORK = new Float64Array( 4 );
*
* dsbtrd.ndarray( 'none', 'upper', 4, 1, AB, 1, 2, 0, d, 1, 0, e, 1, 0, Q, 1, 1, 0, WORK, 1, 0 );
* // d => <Float64Array>[ 4.0, 5.0, 6.0, 7.0 ]
* // e => <Float64Array>[ 1.0, 2.0, 3.0 ]
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dsbtrd;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsbtrd = main;
} else {
	dsbtrd = tmp;
}


// EXPORTS //

module.exports = dsbtrd;

// exports: { "ndarray": "dsbtrd.ndarray" }
