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
* Return sqrt(x^2 + y^2 + z^2) safely avoiding overflow.
*
* @module @stdlib/lapack/base/dlapy3
*
* @example
* var dlapy3 = require( '@stdlib/lapack/base/dlapy3' );
*
* dlapy3( 1.0, 1.0, 1.0 );
*
* @example
* var dlapy3 = require( '@stdlib/lapack/base/dlapy3' );
*
* dlapy3.ndarray( 1.0, 1.0, 1.0 );
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dlapy3.ndarray" }
