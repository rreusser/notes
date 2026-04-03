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
* Multiply a complex vector by the reciprocal of a complex scalar.
*
* @module @stdlib/lapack/base/zrscl
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Complex128 = require( '@stdlib/complex/float64/ctor' );
* var zrscl = require( '@stdlib/lapack/base/zrscl' );
*
* var x = new Complex128Array( [ 6.0, 8.0, 12.0, 16.0 ] );
* var a = new Complex128( 2.0, 0.0 );
*
* zrscl( 2, a, x, 1 );
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Complex128 = require( '@stdlib/complex/float64/ctor' );
* var zrscl = require( '@stdlib/lapack/base/zrscl' );
*
* var x = new Complex128Array( [ 6.0, 8.0, 12.0, 16.0 ] );
* var a = new Complex128( 2.0, 0.0 );
*
* zrscl.ndarray( 2, a, x, 1, 0 );
*/


// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zrscl.ndarray" }
