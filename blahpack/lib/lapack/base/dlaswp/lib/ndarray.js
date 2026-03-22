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
* TODO: Add description.
*
* @param {TODO} N - TODO
* @param {TODO} A - TODO
* @param {TODO} strideA1 - TODO
* @param {TODO} strideA2 - TODO
* @param {TODO} offsetA - TODO
* @param {TODO} k1 - TODO
* @param {TODO} k2 - TODO
* @param {TODO} IPIV - TODO
* @param {TODO} strideIPIV - TODO
* @param {TODO} offsetIPIV - TODO
* @param {TODO} incx - TODO
* @returns {TODO} TODO
*/
function dlaswp( N, A, strideA1, strideA2, offsetA, k1, k2, IPIV, strideIPIV, offsetIPIV, incx ) {
	// TODO: add parameter validation
	return base( N, A, strideA1, strideA2, offsetA, k1, k2, IPIV, strideIPIV, offsetIPIV, incx );
}


// EXPORTS //

module.exports = dlaswp;
