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
* Reduce NB columns of a complex matrix in Hessenberg form.
*
* @module @stdlib/lapack/base/zlahr2
*
* @example
* var zlahr2 = require( '@stdlib/lapack/base/zlahr2' );
*
* zlahr2(  );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var zlahr2 = require( '@stdlib/lapack/base/zlahr2' );
*
* var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var tau = new Float64Array( [ 1.0, 2.0 ] );
* var t = new Float64Array( [ 1.0, 2.0 ] );
* var y = new Float64Array( [ 1.0, 2.0 ] );
*
* zlahr2.ndarray( 2, 2, 2, A, 1, 2, 0, tau, 1, 0, t, 1, 0, 2, y, 1, 0, 2 );
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zlahr2.ndarray" }
