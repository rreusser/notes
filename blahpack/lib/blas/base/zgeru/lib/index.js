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
* Perform complex rank-1 update (unconjugated).
*
* @module @stdlib/blas/base/zgeru
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zgeru = require( '@stdlib/blas/base/zgeru' );
*
* var x = new Complex128Array( [ 1.0, 2.0 ] );
* var y = new Complex128Array( [ 1.0, 2.0 ] );
* var A = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* zgeru( 'row-major', 2, 2, 1.0, x, 1, y, 1, A, 2 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zgeru = require( '@stdlib/blas/base/zgeru' );
*
* var x = new Float64Array( [ 1.0, 2.0 ] );
* var y = new Float64Array( [ 1.0, 2.0 ] );
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* zgeru.ndarray( 2, 2, 1.0, x, 1, 0, y, 1, 0, A, 1, 2, 0 );
*/


// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zgeru.ndarray" }
