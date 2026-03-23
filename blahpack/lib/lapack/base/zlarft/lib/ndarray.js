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

var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Form the triangular factor T of a complex block reflector H of order N,.
*
* @param {string} direct - 'F' for forward, 'B' for backward
* @param {string} storev - 'C' for columnwise, 'R' for rowwise
* @param {NonNegativeInteger} N - order of the block reflector
* @param {NonNegativeInteger} K - number of elementary reflectors
* @param {Complex128Array} V - matrix of reflector vectors
* @param {integer} strideV1 - stride of first dim of V (complex elements)
* @param {integer} strideV2 - stride of second dim of V (complex elements)
* @param {NonNegativeInteger} offsetV - starting index for V (in complex elements)
* @param {Complex128Array} TAU - array of scalar factors
* @param {integer} strideTAU - stride for TAU (in complex elements)
* @param {NonNegativeInteger} offsetTAU - starting index for TAU (in complex elements)
* @param {Complex128Array} T - output triangular matrix
* @param {integer} strideT1 - stride of first dim of T (complex elements)
* @param {integer} strideT2 - stride of second dim of T (complex elements)
* @param {NonNegativeInteger} offsetT - starting index for T (in complex elements)
* @throws {TypeError} first argument must be a valid direction
* @throws {TypeError} second argument must be a valid storage direction
* @throws {RangeError} third argument must be a nonnegative integer
* @throws {RangeError} fourth argument must be a nonnegative integer
* @returns {*} result
*/
function zlarft( direct, storev, N, K, V, strideV1, strideV2, offsetV, TAU, strideTAU, offsetTAU, T, strideT1, strideT2, offsetT ) {
	if ( direct !== 'forward' && direct !== 'backward' ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid direction. Value: `%s`.', direct ) );
	}
	if ( storev !== 'column-wise' && storev !== 'row-wise' ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid storage direction. Value: `%s`.', storev ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Third argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( K < 0 ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be a nonnegative integer. Value: `%d`.', K ) );
	}
	if ( N === 0 ) {
		return;
	}
	return base( direct, storev, N, K, V, strideV1, strideV2, offsetV, TAU, strideTAU, offsetTAU, T, strideT1, strideT2, offsetT );
}


// EXPORTS //

module.exports = zlarft;
