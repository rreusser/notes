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
* Overwrite the M-by-N matrix C with Q.
*
* @param {string} side - 'L' to apply Q from left, 'R' from right
* @param {string} trans - 'N' for Q, 'C' for Q^H
* @param {NonNegativeInteger} M - number of rows of C
* @param {NonNegativeInteger} N - number of columns of C
* @param {NonNegativeInteger} K - number of elementary reflectors
* @param {Complex128Array} A - reflector vectors from ZGELQF (stored rowwise)
* @param {integer} strideA1 - stride of the first dimension of A (complex elements)
* @param {integer} strideA2 - stride of the second dimension of A (complex elements)
* @param {NonNegativeInteger} offsetA - starting index for A (complex elements)
* @param {Complex128Array} TAU - scalar factors of reflectors
* @param {integer} strideTAU - stride for TAU (complex elements)
* @param {NonNegativeInteger} offsetTAU - starting index for TAU (complex elements)
* @param {Complex128Array} C - input/output matrix
* @param {integer} strideC1 - stride of the first dimension of C (complex elements)
* @param {integer} strideC2 - stride of the second dimension of C (complex elements)
* @param {NonNegativeInteger} offsetC - starting index for C (complex elements)
* @param {Complex128Array} WORK - workspace
* @param {integer} strideWORK - stride for WORK (complex elements)
* @param {NonNegativeInteger} offsetWORK - starting index for WORK (complex elements)
* @param {integer} lwork - workspace size in complex elements (unused, kept for API compat)
* @throws {TypeError} first argument must be a valid operation side
* @throws {TypeError} second argument must be a valid transpose operation
* @throws {RangeError} third argument must be a nonnegative integer
* @throws {RangeError} fourth argument must be a nonnegative integer
* @throws {RangeError} fifth argument must be a nonnegative integer
* @returns {integer} info - 0 if successful
*/
function zunmlq( side, trans, M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK, lwork ) {
	if ( !isOperationSide( side ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid operation side. Value: `%s`.', side ) );
	}
	if ( !isMatrixTranspose( trans ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid transpose operation. Value: `%s`.', trans ) );
	}
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. Third argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( K < 0 ) {
		throw new RangeError( format( 'invalid argument. Fifth argument must be a nonnegative integer. Value: `%d`.', K ) );
	}
	if ( M === 0 || N === 0 ) {
		return 0;
	}
	return base( side, trans, M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK, lwork );
}


// EXPORTS //

module.exports = zunmlq;
