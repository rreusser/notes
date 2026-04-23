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

var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zsptrs = require( './../lib' );

// Solve a 2x2 symmetric system A*x = b using pre-factored packed storage.

// Factored upper-triangle AP for a 2x2 symmetric matrix with trivial pivoting:
var AP = new Complex128Array( [ 4.0, 1.0, 1.0, 2.0, 5.0, -1.0 ] );
var IPIV = new Int32Array( [ 0, 1 ] );
var B = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0 ] );

var info = zsptrs.ndarray( 'upper', 2, 1, AP, 1, 0, IPIV, 1, 0, B, 1, 2, 0 );
var Bv = reinterpret( B, 0 );

console.log( 'info = %d', info );
console.log( 'Solution: [(%f, %f), (%f, %f)]', Bv[ 0 ], Bv[ 1 ], Bv[ 2 ], Bv[ 3 ] );
