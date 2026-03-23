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
* Compute Cholesky factorization of a Hermitian positive definite banded matrix (unblocked).
*
* @module @stdlib/lapack/base/zpbtf2
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zpbtf2 = require( '@stdlib/lapack/base/zpbtf2' );
*
* var AB = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* zpbtf2( 'row-major', 'upper', 2, 2, AB, 2 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zpbtf2 = require( '@stdlib/lapack/base/zpbtf2' );
*
* var AB = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
*
* zpbtf2.ndarray( 'upper', 2, 2, AB, 1, 2, 0 );
*/


// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zpbtf2.ndarray" }
