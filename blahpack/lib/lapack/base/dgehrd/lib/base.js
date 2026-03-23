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

var daxpy = require( '../../../../blas/base/daxpy/lib/base.js' );
var dgehd2 = require( '../../dgehd2/lib/base.js' );
var dgemm = require( '../../../../blas/base/dgemm/lib/base.js' );
var dlahr2 = require( '../../dlahr2/lib/base.js' );
var dlarfb = require( '../../dlarfb/lib/base.js' );
var dtrmm = require( '../../../../blas/base/dtrmm/lib/base.js' );

// VARIABLES //

var NBMAX = 64;
var LDT = NBMAX + 1;

// MAIN //

/**
* Reduces a real general matrix to upper Hessenberg form (blocked).
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
* @param {integer} lwork - length of WORK
* @returns {integer} status code (0 = success)
*/
function dgehrd( N, ilo, ihi, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK, lwork ) {
	var LDWORK;
	var ei;
	var IWT;
	var NB;
	var NX;
	var NH;
	var IB;
	var i;
	var j;

	NB = 32;

	// Set elements 1:ILO-1 and IHI:N-1 of TAU to zero (0-based loop)
	for ( i = 0; i < ilo - 1; i++ ) {
		TAU[ offsetTAU + i * strideTAU ] = 0.0;
	}
	for ( i = Math.max( 0, ihi - 1 ); i < N - 1; i++ ) {
		TAU[ offsetTAU + i * strideTAU ] = 0.0;
	}

	// Quick return if possible
	NH = ihi - ilo + 1;
	if ( NH <= 1 ) {
		return 0;
	}

	// Determine the block size
	NX = Math.max( NB, NB );
	LDWORK = N;

	if ( NB < 2 || NB >= NH ) {
		// Use unblocked code below
		i = ilo;
	} else {
		// Use blocked code
		IWT = N * NB;
		for ( i = ilo; i <= ihi - 1 - NX; i += NB ) {
			IB = Math.min( NB, ihi - i );

			// Reduce columns i:i+ib-1 to Hessenberg form, returning the
			// matrices V and T of the block reflector H = I - V*T*V**T
			// which performs the reduction, and also the matrix Y = A*V*T
			dlahr2( ihi, i, IB, A, strideA1, strideA2, offsetA + ( i - 1 ) * strideA2, TAU, strideTAU, offsetTAU + ( i - 1 ) * strideTAU, WORK, 1, LDT, offsetWORK + IWT, WORK, 1, LDWORK, offsetWORK );

			// Apply the block reflector H to A(1:ihi,i+ib:ihi) from the
			// right, computing A := A - Y * V**T. V(i+ib,ib-1) must be set to 1.
			ei = A[ offsetA + ( i + IB - 1 ) * strideA1 + ( i + IB - 2 ) * strideA2 ];
			A[ offsetA + ( i + IB - 1 ) * strideA1 + ( i + IB - 2 ) * strideA2 ] = 1.0;

			dgemm( 'no-transpose', 'transpose', ihi, ihi - i - IB + 1, IB, -1.0, WORK, 1, LDWORK, offsetWORK, A, strideA1, strideA2, offsetA + ( i + IB - 1 ) * strideA1 + ( i - 1 ) * strideA2, 1.0, A, strideA1, strideA2, offsetA + ( i + IB - 1 ) * strideA2 );

			A[ offsetA + ( i + IB - 1 ) * strideA1 + ( i + IB - 2 ) * strideA2 ] = ei;

			// Apply the block reflector H to A(1:i,i+1:i+ib-1) from the right
			dtrmm( 'right', 'lower', 'transpose', 'unit', i, IB - 1, 1.0, A, strideA1, strideA2, offsetA + i * strideA1 + ( i - 1 ) * strideA2, WORK, 1, LDWORK, offsetWORK );

			for ( j = 0; j < IB - 1; j++ ) {
				daxpy( i, -1.0, WORK, 1, offsetWORK + LDWORK * j, A, strideA1, offsetA + ( i + j ) * strideA2 );
			}

			// Apply the block reflector H to A(i+1:ihi,i+ib:n) from the left
			dlarfb( 'left', 'transpose', 'forward', 'columnwise', ihi - i, N - i - IB + 1, IB, A, strideA1, strideA2, offsetA + i * strideA1 + ( i - 1 ) * strideA2, WORK, 1, LDT, offsetWORK + IWT, A, strideA1, strideA2, offsetA + i * strideA1 + ( i + IB - 1 ) * strideA2, WORK, 1, LDWORK, offsetWORK );
		}
	}

	// Use unblocked code to reduce the rest of the matrix
	dgehd2( N, i, ihi, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK );

	return 0;
}


// EXPORTS //

module.exports = dgehrd;
