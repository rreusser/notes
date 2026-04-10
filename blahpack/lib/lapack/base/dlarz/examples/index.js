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

var discreteUniform = require( '@stdlib/random/array/discrete-uniform' );
var dlarz = require( './../lib' );

var opts = {
	'dtype': 'float64'
};
var M = 4;
var N = 4;
var L = 2;
var C = discreteUniform( M * N, -10, 10, opts );
var v = discreteUniform( L, -10, 10, opts );
var WORK = discreteUniform( N, -10, 10, opts );

// Using the standard interface:
dlarz( 'column-major', 'left', M, N, L, v, 1, 0.5, C, M, WORK, 1 );
console.log( C );

// Using the ndarray interface:
dlarz.ndarray( 'left', M, N, L, v, 1, 0, 0.5, C, 1, M, 0, WORK, 1, 0 );
console.log( C );
