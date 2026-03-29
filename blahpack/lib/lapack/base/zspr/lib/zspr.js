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

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* Performs the symmetric rank-1 update `A := alpha*x*x**T + A`.
*
* @param {string} uplo - specifies whether the upper or lower triangle is stored
* @param {NonNegativeInteger} N - order of the matrix `A`
* @param {Complex128} alpha - complex scalar constant
* @param {Complex128Array} x - complex input vector
* @param {integer} strideX - stride length for `x` (in complex elements)
* @param {Complex128Array} AP - packed symmetric matrix
* @param {integer} strideAP - stride length for `AP` (in complex elements)
* @returns {Complex128Array} `AP`
*/
function zspr( uplo, N, alpha, x, strideX, AP, strideAP ) { // eslint-disable-line max-len, max-params
	var ox;

	ox = stride2offset( N, strideX );
	return base( uplo, N, alpha, x, strideX, ox, AP, strideAP, 0 );
}


// EXPORTS //

module.exports = zspr;
