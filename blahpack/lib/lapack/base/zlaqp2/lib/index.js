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
* QR factorization with column pivoting (unblocked).
*
* @module @stdlib/lapack/base/zlaqp2
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zlaqp2 = require( '@stdlib/lapack/base/zlaqp2' );
*
* var A = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var JPVT = new Int32Array( 2 );
* var TAU = new Complex128Array( [ 1.0, 2.0 ] );
* var VN1 = new Float64Array( [ 1.0, 2.0 ] );
* var VN2 = new Float64Array( [ 1.0, 2.0 ] );
* var WORK = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
*
* zlaqp2( 'row-major', 2, 2, 0, A, 2, JPVT, 1, TAU, 1, VN1, 1, VN2, 2, WORK, 1 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zlaqp2 = require( '@stdlib/lapack/base/zlaqp2' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var JPVT = new Int32Array( 2 );
* var TAU = new Float64Array( [ 1.0, 2.0 ] );
* var VN1 = new Float64Array( [ 1.0, 2.0 ] );
* var VN2 = new Float64Array( [ 1.0, 2.0 ] );
* var WORK = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
*
* zlaqp2.ndarray( 2, 2, 0, A, 1, 2, 0, JPVT, 1, 0, TAU, 1, 0, VN1, 1, 0, VN2, 2, 0, WORK, 1, 0 );
*/


// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zlaqp2.ndarray" }
