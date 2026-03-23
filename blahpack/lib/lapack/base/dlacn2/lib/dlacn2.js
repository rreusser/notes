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
* Estimates the 1-norm of a square matrix using reverse communication.
*
* @param {NonNegativeInteger} N - TODO
* @param {Float64Array} v - input array
* @param {integer} strideV - `v` stride length
* @param {Float64Array} x - input array
* @param {integer} strideX - `x` stride length
* @param {Int32Array} ISGN - input array
* @param {integer} strideISGN - `ISGN` stride length
* @param {Float64Array} EST - TODO
* @param {Int32Array} KASE - TODO
* @param {Int32Array} ISAVE - input array
* @param {integer} strideISAVE - `ISAVE` stride length
* @returns {*} result
*/
function dlacn2( N, v, strideV, x, strideX, ISGN, strideISGN, EST, KASE, ISAVE, strideISAVE ) {
	var ov = stride2offset( N, strideV );
	var ox = stride2offset( N, strideX );
	var oi = stride2offset( N, strideISGN );
	var oi = stride2offset( N, strideISAVE );
	return base( N, v, strideV, ov, x, strideX, ox, ISGN, strideISGN, oi, EST, KASE, ISAVE, strideISAVE, oi );
}


// EXPORTS //

module.exports = dlacn2;
