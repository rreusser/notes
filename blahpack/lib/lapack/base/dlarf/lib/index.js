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
* Apply a real Householder reflector to a matrix.
*
* @module @stdlib/lapack/base/dlarf
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlarf = require( '@stdlib/lapack/base/dlarf' );
*
* var v = new Float64Array( [ 1.0, 2.0 ] );
* var C = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var WORK = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
*
* dlarf( 'row-major', 'left', 2, 2, v, 1, 1.0, C, 2, WORK, 1 );
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlarf = require( '@stdlib/lapack/base/dlarf' );
*
* var v = new Float64Array( [ 1.0, 2.0 ] );
* var C = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var WORK = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
*
* dlarf.ndarray( 'left', 2, 2, v, 1, 0, 1.0, C, 1, 2, 0, WORK, 1, 0 );
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dlarf.ndarray" }
