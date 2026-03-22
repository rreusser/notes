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
* Return sqrt(x^2 + y^2 + z^2) safely avoiding overflow
*
* @param {number} x - x
* @param {number} y - y
* @param {number} z - z
* @returns {number} result
*/
function dlapy3( x, y, z ) {
	return base( x, y, z );
}


// EXPORTS //

module.exports = dlapy3;
