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
* Performs a Hermitian rank-k update (complex double-precision).
*
* @module @stdlib/blas/base/zherk
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zherk = require( '@stdlib/blas/base/zherk' );
* 
* var A = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var C = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* 
* zherk( 'row-major', 'upper', 'no-transpose', 2, 2, 1.0, A, 2, 0.0, C, 2 );
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zherk = require( '@stdlib/blas/base/zherk' );
* 
* var A = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var C = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* 
* zherk.ndarray( 'upper', 'no-transpose', 2, 2, 1.0, A, 1, 2, 0, 0.0, C, 1, 2, 0 );
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zherk.ndarray" }
