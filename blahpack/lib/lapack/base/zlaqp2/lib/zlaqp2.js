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

var isLayout = require( '@stdlib/blas/base/assert/is-layout' );
var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Computes a QR factorization with column pivoting of the M-by-N matrix A.
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {NonNegativeInteger} M - TODO
* @param {NonNegativeInteger} N - TODO
* @param {NonNegativeInteger} offset - TODO
* @param {Complex128Array} A - input matrix
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Int32Array} JPVT - input array
* @param {integer} strideJPVT - `JPVT` stride length
* @param {Complex128Array} TAU - input array
* @param {integer} strideTAU - `TAU` stride length
* @param {Float64Array} VN1 - input array
* @param {integer} strideVN1 - `VN1` stride length
* @param {Float64Array} VN2 - input array
* @param {integer} strideVN2 - `VN2` stride length
* @param {Complex128Array} WORK - input array
* @param {integer} strideWORK - `WORK` stride length
* @throws {TypeError} first argument must be a valid order
* @returns {*} result
*/
function zlaqp2( order, M, N, offset, A, LDA, JPVT, strideJPVT, TAU, strideTAU, VN1, strideVN1, VN2, strideVN2, WORK, strideWORK ) {
	var sa1;
	var sa2;
	var oj;
	var ot;
	var ov;
	var ov;
	var ow;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( order === 'column-major' ) {
		sa1 = 1;
		sa2 = LDA;
	} else {
		sa1 = LDA;
		sa2 = 1;
	}
	oj = stride2offset( N, strideJPVT );
	ot = stride2offset( N, strideTAU );
	ov = stride2offset( N, strideVN1 );
	ov = stride2offset( N, strideVN2 );
	ow = stride2offset( N, strideWORK );
	return base( M, N, offset, A, sa1, sa2, 0, JPVT, strideJPVT, oj, TAU, strideTAU, ot, VN1, strideVN1, ov, VN2, strideVN2, ov, WORK, strideWORK, ow );
}


// EXPORTS //

module.exports = zlaqp2;
