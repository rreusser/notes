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

var isMatrixTranspose = require( '@stdlib/blas/base/assert/is-transpose-operation' );
var isOperationSide = require( '@stdlib/blas/base/assert/is-operation-side' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Apply a complex block reflector H or its conjugate-transpose H^H to a.
*
* @param {string} side - 'L' or 'R'
* @param {string} trans - 'N' or 'C'
* @param {string} direct - 'F' or 'B'
* @param {string} storev - 'C' or 'R'
* @param {NonNegativeInteger} M - rows of C
* @param {NonNegativeInteger} N - columns of C
* @param {NonNegativeInteger} K - number of elementary reflectors
* @param {Complex128Array} V - matrix of reflector vectors
* @param {integer} strideV1 - first dim stride of V (complex elements)
* @param {integer} strideV2 - second dim stride of V (complex elements)
* @param {NonNegativeInteger} offsetV - starting index for V (in complex elements)
* @param {Complex128Array} T - triangular factor
* @param {integer} strideT1 - first dim stride of T (complex elements)
* @param {integer} strideT2 - second dim stride of T (complex elements)
* @param {NonNegativeInteger} offsetT - starting index for T (in complex elements)
* @param {Complex128Array} C - matrix, modified in-place
* @param {integer} strideC1 - first dim stride of C (complex elements)
* @param {integer} strideC2 - second dim stride of C (complex elements)
* @param {NonNegativeInteger} offsetC - starting index for C (in complex elements)
* @param {Complex128Array} WORK - workspace
* @param {integer} strideWORK1 - first dim stride of WORK (complex elements)
* @param {integer} strideWORK2 - second dim stride of WORK (complex elements)
* @param {NonNegativeInteger} offsetWORK - starting index for WORK (in complex elements)
* @throws {TypeError} first argument must be a valid operation side
* @throws {TypeError} second argument must be a valid transpose operation
* @throws {TypeError} third argument must be a valid direction
* @throws {TypeError} fourth argument must be a valid storage direction
* @throws {RangeError} fifth argument must be a nonnegative integer
* @throws {RangeError} sixth argument must be a nonnegative integer
* @throws {RangeError} seventh argument must be a nonnegative integer
* @returns {*} result
*/
function zlarfb( side, trans, direct, storev, M, N, K, V, strideV1, strideV2, offsetV, T, strideT1, strideT2, offsetT, C, strideC1, strideC2, offsetC, WORK, strideWORK1, strideWORK2, offsetWORK ) {
	if ( !isOperationSide( side ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid operation side. Value: `%s`.', side ) );
	}
	if ( !isMatrixTranspose( trans ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid transpose operation. Value: `%s`.', trans ) );
	}
	if ( direct !== 'forward' && direct !== 'backward' ) {
		throw new TypeError( format( 'invalid argument. Third argument must be a valid direction. Value: `%s`.', direct ) );
	}
	if ( storev !== 'column-wise' && storev !== 'row-wise' ) {
		throw new TypeError( format( 'invalid argument. Fourth argument must be a valid storage direction. Value: `%s`.', storev ) );
	}
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. Fifth argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Sixth argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( K < 0 ) {
		throw new RangeError( format( 'invalid argument. Seventh argument must be a nonnegative integer. Value: `%d`.', K ) );
	}
	if ( M === 0 || N === 0 ) {
		return;
	}
	return base( side, trans, direct, storev, M, N, K, V, strideV1, strideV2, offsetV, T, strideT1, strideT2, offsetT, C, strideC1, strideC2, offsetC, WORK, strideWORK1, strideWORK2, offsetWORK );
}


// EXPORTS //

module.exports = zlarfb;
