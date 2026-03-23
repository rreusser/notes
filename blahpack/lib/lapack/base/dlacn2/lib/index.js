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
* Estimate the 1-norm of a square matrix using reverse communication.
*
* @module @stdlib/lapack/base/dlacn2
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dlacn2 = require( '@stdlib/lapack/base/dlacn2' );
* 
* var v = new Float64Array( [ 1.0, 2.0 ] );
* var x = new Float64Array( [ 1.0, 2.0 ] );
* var ISGN = new Int32Array( 2 );
* var EST = new Float64Array( [ 1.0, 2.0 ] );
* var KASE = new Int32Array( 2 );
* var ISAVE = new Int32Array( 2 );
* 
* dlacn2( 2, v, 1, x, 1, ISGN, 1, EST, KASE, ISAVE, 1 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dlacn2 = require( '@stdlib/lapack/base/dlacn2' );
* 
* var v = new Float64Array( [ 1.0, 2.0 ] );
* var x = new Float64Array( [ 1.0, 2.0 ] );
* var ISGN = new Int32Array( 2 );
* var isave = new Int32Array( 2 );
* 
* dlacn2.ndarray( 2, v, 1, 0, x, 1, 0, ISGN, 1, 0, 1.0, 2, isave, 1, 0 );
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dlacn2.ndarray" }
