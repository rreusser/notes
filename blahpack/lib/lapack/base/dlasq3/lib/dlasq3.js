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
* Checks for deflation, computes a shift (TAU) and calls dqds. In case of.
*
* @param {integer} i0 - TODO
* @param {integer} n0 - TODO
* @param {Float64Array} z - input array
* @param {integer} stride - `z` stride length
* @param {integer} pp - TODO
* @param {number} dmin - TODO
* @param {number} sigma - TODO
* @param {number} desig - TODO
* @param {number} qmax - TODO
* @param {integer} nfail - TODO
* @param {integer} iter - TODO
* @param {integer} ndiv - TODO
* @param {integer} ttype - TODO
* @param {number} dmin1 - TODO
* @param {number} dmin2 - TODO
* @param {number} dn - TODO
* @param {number} dn1 - TODO
* @param {number} dn2 - TODO
* @param {number} g - TODO
* @param {number} tau - TODO
* @returns {*} result
*/
function dlasq3( i0, n0, z, stride, pp, dmin, sigma, desig, qmax, nfail, iter, ndiv, ieee, ttype, dmin1, dmin2, dn, dn1, dn2, g, tau ) {
	var oz = stride2offset( N, stride );
	return base( i0, n0, z, stride, oz, pp, dmin, sigma, desig, qmax, nfail, iter, ndiv, ieee, ttype, dmin1, dmin2, dn, dn1, dn2, g, tau );
}


// EXPORTS //

module.exports = dlasq3;
