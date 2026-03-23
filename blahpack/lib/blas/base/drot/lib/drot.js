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

/* eslint-disable max-len, max-params */

'use strict';


// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* Applies a plane rotation.
*
* @param {NonNegativeInteger} N - TODO
* @param {Float64Array} x - input array
* @param {integer} strideX - `x` stride length
* @param {Float64Array} y - input array
* @param {integer} strideY - `y` stride length
* @param {number} c - TODO
* @param {number} s - TODO
* @returns {*} result
*/
function drot( N, x, strideX, y, strideY, c, s ) {
	var ox = stride2offset( N, strideX );
	var oy = stride2offset( N, strideY );
	return base( N, x, strideX, ox, y, strideY, oy, c, s );
}


// EXPORTS //

module.exports = drot;
