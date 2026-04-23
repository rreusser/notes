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
var Float64Array = require( '@stdlib/array/float64' );
var zgges = require( './../lib' );

// 3x3 complex diagonal matrices (interleaved re/im, column-major):
var A = new Complex128Array( new Float64Array([
	1,
	1,
	0,
	0,
	0,
	0,
	0,
	0,
	2,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	3,
	-1
]) );
var B = new Complex128Array( new Float64Array([
	1,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	1,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	1,
	0
]) );
var ALPHA = new Complex128Array( 3 );
var BETA = new Complex128Array( 3 );
var VSL = new Complex128Array( 9 );
var VSR = new Complex128Array( 9 );
var result;

/**
* Dummy selection function that selects no eigenvalues.
*
* @private
* @returns {boolean} false
*/
function noop() {
	return false;
}

result = zgges( 'compute-vectors', 'compute-vectors', 'not-sorted', noop, 3, A, 1, 3, 0, B, 1, 3, 0, ALPHA, 1, 0, BETA, 1, 0, VSL, 1, 3, 0, VSR, 1, 3, 0 ); // eslint-disable-line max-len

console.log( 'info:', result.info ); // eslint-disable-line no-console
console.log( 'sdim:', result.sdim ); // eslint-disable-line no-console
