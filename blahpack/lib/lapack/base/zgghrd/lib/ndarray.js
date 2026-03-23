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
* Reduce a pair of complex matrices (A, B) to generalized upper Hessenberg.
*
* @param {string} compq - 'N': do not compute Q; 'I': initialize Q to identity and compute; 'V': accumulate into Q
* @param {string} compz - 'N': do not compute Z; 'I': initialize Z to identity and compute; 'V': accumulate into Z
* @param {NonNegativeInteger} N - order of the matrices A and B
* @param {integer} ilo - ilo (1-based)
* @param {integer} ihi - ihi (1-based)
* @param {Complex128Array} A - input/output matrix A
* @param {integer} strideA1 - stride of the first dimension of `A` (complex elements)
* @param {integer} strideA2 - stride of the second dimension of `A` (complex elements)
* @param {NonNegativeInteger} offsetA - starting index for `A` (complex elements)
* @param {Complex128Array} B - input/output matrix B
* @param {integer} strideB1 - stride of the first dimension of `B` (complex elements)
* @param {integer} strideB2 - stride of the second dimension of `B` (complex elements)
* @param {NonNegativeInteger} offsetB - starting index for `B` (complex elements)
* @param {Complex128Array} Q - input/output matrix Q
* @param {integer} strideQ1 - stride of the first dimension of `Q` (complex elements)
* @param {integer} strideQ2 - stride of the second dimension of `Q` (complex elements)
* @param {NonNegativeInteger} offsetQ - starting index for `Q` (complex elements)
* @param {Complex128Array} Z - input/output matrix Z
* @param {integer} strideZ1 - stride of the first dimension of `Z` (complex elements)
* @param {integer} strideZ2 - stride of the second dimension of `Z` (complex elements)
* @param {NonNegativeInteger} offsetZ - starting index for `Z` (complex elements)
* @throws {TypeError} first argument must be a valid computation flag
* @throws {TypeError} second argument must be a valid computation flag
* @throws {RangeError} third argument must be a nonnegative integer
* @returns {integer} status code (0 = success)
*/
function zgghrd( compq, compz, N, ilo, ihi, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, Q, strideQ1, strideQ2, offsetQ, Z, strideZ1, strideZ2, offsetZ ) {
	if ( compq !== 'none' && compq !== 'initialize' && compq !== 'compute' ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid computation flag. Value: `%s`.', compq ) );
	}
	if ( compz !== 'none' && compz !== 'initialize' && compz !== 'compute' ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid computation flag. Value: `%s`.', compz ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Third argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( N === 0 ) {
		return 0;
	}
	return base( compq, compz, N, ilo, ihi, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, Q, strideQ1, strideQ2, offsetQ, Z, strideZ1, strideZ2, offsetZ );
}


// EXPORTS //

module.exports = zgghrd;
