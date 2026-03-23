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
* Computes the singular values of a 2-by-2 triangular matrix:.
*
* @param {number} f - the (1,1) element of the 2-by-2 matrix
* @param {number} g - the (1,2) element of the 2-by-2 matrix
* @param {number} h - the (2,2) element of the 2-by-2 matrix
* @param {Float64Array} out - output array: out[0]=ssmin, out[1]=ssmax
* @returns {Float64Array} out
*/
function dlas2( f, g, h, out ) {
	return base( f, g, h, out );
}


// EXPORTS //

module.exports = dlas2;
