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
* Estimates the reciprocal condition number of a real symmetric positive definite matrix in packed storage.
*
* @module @stdlib/lapack/base/dppcon
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dppcon = require( '@stdlib/lapack/base/dppcon' );
*
* var AP = new Float64Array( [ 2, 0, 2, 0, 0, 2 ] );
* var rcond = new Float64Array( 1 );
* var WORK = new Float64Array( 9 );
* var IWORK = new Int32Array( 3 );
*
* var info = dppcon( 'upper', 3, AP, 2.0, rcond, WORK, IWORK );
* // returns 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dppcon.ndarray" }
