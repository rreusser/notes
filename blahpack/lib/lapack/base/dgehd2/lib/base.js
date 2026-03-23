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

var dlarf = require( '../../dlarf/lib/base.js' );
var dlarfg = require( '../../dlarfg/lib/base.js' );

// VARIABLES //

var SCRATCH_ALPHA = new Float64Array( 1 );

// MAIN //

/**
* Reduce a general matrix to upper Hessenberg form (unblocked).
*
* @private
* @param {NonNegativeInteger} N - number of columns
* @param {integer} ilo - ilo (1-based)
* @param {integer} ihi - ihi (1-based)
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} TAU - output array
* @param {integer} strideTAU - stride length for `TAU`
* @param {NonNegativeInteger} offsetTAU - starting index for `TAU`
* @param {Float64Array} WORK - workspace array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @returns {integer} status code (0 = success)
*/
function dgehd2( N, ilo, ihi, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK ) {
	var xStart;
	var oAlpha;
	var oTau;
	var aii;
	var i;

	for ( i = ilo - 1; i < ihi - 1; i++ ) {
		// Save A(i+1, i) into alpha scratch, then compute reflector
		oAlpha = offsetA + ( i + 1 ) * strideA1 + i * strideA2;
		SCRATCH_ALPHA[ 0 ] = A[ oAlpha ];
		xStart = Math.min( i + 2, N - 1 );

		// TAU is written directly by dlarfg
		oTau = offsetTAU + i * strideTAU;
		dlarfg( ihi - i - 1, SCRATCH_ALPHA, 0, A, strideA1, offsetA + xStart * strideA1 + i * strideA2, TAU, oTau );

		// Set A(i+1, i) = 1 for the reflector application
		aii = A[ oAlpha ];
		A[ oAlpha ] = 1.0;

		// Apply H(i) to A(1:ihi, i+1:ihi) from the right
		dlarf( 'right', ihi, ihi - i - 1, A, strideA1, offsetA + ( i + 1 ) * strideA1 + i * strideA2, TAU[ oTau ], A, strideA1, strideA2, offsetA + ( i + 1 ) * strideA2, WORK, strideWORK, offsetWORK );

		// Apply H(i) to A(i+1:ihi, i+1:n) from the left
		dlarf( 'left', ihi - i - 1, N - i - 1, A, strideA1, offsetA + ( i + 1 ) * strideA1 + i * strideA2, TAU[ oTau ], A, strideA1, strideA2, offsetA + ( i + 1 ) * strideA1 + ( i + 1 ) * strideA2, WORK, strideWORK, offsetWORK );

		// Restore A(i+1, i) = alpha (the sub-diagonal element)
		A[ oAlpha ] = SCRATCH_ALPHA[ 0 ];
	}
	return 0;
}


// EXPORTS //

module.exports = dgehd2;
