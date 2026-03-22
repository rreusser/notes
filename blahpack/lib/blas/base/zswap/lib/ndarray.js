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
* Interchange two complex double-precision vectors.
*
* @param {PositiveInteger} N - number of complex elements
* @param {Float64Array} zx - first input array (interleaved complex)
* @param {integer} strideX - stride for `zx` (in complex elements)
* @param {NonNegativeInteger} offsetX - starting index for `zx`
* @param {Float64Array} zy - second input array (interleaved complex)
* @param {integer} strideY - stride for `zy` (in complex elements)
* @param {NonNegativeInteger} offsetY - starting index for `zy`
* @returns {Float64Array} `zx`
*/
function zswap( N, zx, strideX, offsetX, zy, strideY, offsetY ) {
	return base( N, zx, strideX, offsetX, zy, strideY, offsetY );
}


// EXPORTS //

module.exports = zswap;
