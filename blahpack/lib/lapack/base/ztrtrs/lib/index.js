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
* Solve a complex triangular system with multiple right-hand sides.
*
* @module @stdlib/lapack/base/ztrtrs
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var ztrtrs = require( '@stdlib/lapack/base/ztrtrs' );
* 
* var A = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var B = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* 
* ztrtrs( 'row-major', 'upper', 'no-transpose', 'non-unit', 2, 1, A, 2, B, 2 );
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var ztrtrs = require( '@stdlib/lapack/base/ztrtrs' );
* 
* var A = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var B = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* 
* ztrtrs.ndarray( 'upper', 'no-transpose', 'non-unit', 2, 1, A, 1, 2, 0, B, 1, 2, 0 );
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "ztrtrs.ndarray" }
