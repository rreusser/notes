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
* Computes all the eigenvalues of the symmetric positive definite tridiagonal.
*
* @param {NonNegativeInteger} N - TODO
* @param {Float64Array} z - input array
* @param {integer} stride - `z` stride length
* @returns {*} result
*/
function dlasq2( N, z, stride ) {
	var oz = stride2offset( N, stride );
	return base( N, z, stride, oz );
}


// EXPORTS //

module.exports = dlasq2;
