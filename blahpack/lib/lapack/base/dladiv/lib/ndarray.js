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

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Performs complex division in real arithmetic:.
*
* @param {number} a - real part of numerator
* @param {number} b - imaginary part of numerator
* @param {number} c - real part of denominator
* @param {number} d - imaginary part of denominator
* @param {Float64Array} out - output array: out[0]=p, out[1]=q
* @returns {Float64Array} out
*/
function dladiv( a, b, c, d, out ) {
	return base( a, b, c, d, out );
}


// EXPORTS //

module.exports = dladiv;
