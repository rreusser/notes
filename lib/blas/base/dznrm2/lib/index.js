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
* Compute the Euclidean norm of a complex double-precision vector.
*
* @module @stdlib/blas/base/dznrm2
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var dznrm2 = require( '@stdlib/blas/base/dznrm2' );
*
* var x = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* var nrm = dznrm2( 2, x, 1, 0 );
* // returns ~5.477
*/

'use strict';

module.exports = require( './base.js' );
