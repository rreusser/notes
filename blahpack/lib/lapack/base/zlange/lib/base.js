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

var zlassq = require( '../../zlassq/lib/base.js' );


// MAIN //

/**
* Computes the value of a matrix norm for a complex matrix.
*
* Supports norms: 'M' (max abs), '1'/'O' (one-norm), 'I' (infinity-norm),
* 'F'/'E' (Frobenius norm).
*
* Complex elements are stored as interleaved real/imaginary pairs.
* Element (i,j) is at offset + 2*(i*strideA1 + j*strideA2).
*
* @private
* @param {string} norm - 'M', '1', 'O', 'I', 'F', or 'E'
* @param {NonNegativeInteger} M - rows
* @param {NonNegativeInteger} N - columns
* @param {Float64Array} A - complex matrix (interleaved)
* @param {integer} strideA1 - first dimension stride
* @param {integer} strideA2 - second dimension stride
* @param {NonNegativeInteger} offsetA - starting index for A
* @param {Float64Array} WORK - workspace (length >= M for 'I' norm)
* @param {integer} strideWORK - stride for WORK
* @param {NonNegativeInteger} offsetWORK - starting index for WORK
* @returns {number} norm value
*/
function zlange( norm, M, N, A, strideA1, strideA2, offsetA, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	var value;
	var scale;
	var temp;
	var sum;
	var out;
	var sa1;
	var sa2;
	var ai;
	var ar;
	var im;
	var wi;
	var i;
	var j;

	if ( M === 0 || N === 0 ) {
		return 0.0;
	}

	// Matrix strides in complex elements, multiply by 2
	sa1 = strideA1 * 2;
	sa2 = strideA2 * 2;

	if ( norm === 'M' || norm === 'm' ) {
		// Max absolute value
		value = 0.0;
		for ( j = 0; j < N; j++ ) {
			for ( i = 0; i < M; i++ ) {
				ai = offsetA + i * sa1 + j * sa2;
				ar = A[ ai ];
				im = A[ ai + 1 ];
				temp = Math.sqrt( ar * ar + im * im );
				if ( value < temp || temp !== temp ) {
					value = temp;
				}
			}
		}
	} else if ( norm === 'O' || norm === 'o' || norm === '1' ) {
		// One-norm: maximum column sum of abs values
		value = 0.0;
		for ( j = 0; j < N; j++ ) {
			sum = 0.0;
			for ( i = 0; i < M; i++ ) {
				ai = offsetA + i * sa1 + j * sa2;
				ar = A[ ai ];
				im = A[ ai + 1 ];
				sum += Math.sqrt( ar * ar + im * im );
			}
			if ( value < sum || sum !== sum ) {
				value = sum;
			}
		}
	} else if ( norm === 'I' || norm === 'i' ) {
		// Infinity-norm: maximum row sum of abs values
		for ( i = 0; i < M; i++ ) {
			wi = offsetWORK + i * strideWORK;
			WORK[ wi ] = 0.0;
		}
		for ( j = 0; j < N; j++ ) {
			for ( i = 0; i < M; i++ ) {
				ai = offsetA + i * sa1 + j * sa2;
				ar = A[ ai ];
				im = A[ ai + 1 ];
				wi = offsetWORK + i * strideWORK;
				WORK[ wi ] += Math.sqrt( ar * ar + im * im );
			}
		}
		value = 0.0;
		for ( i = 0; i < M; i++ ) {
			wi = offsetWORK + i * strideWORK;
			temp = WORK[ wi ];
			if ( value < temp || temp !== temp ) {
				value = temp;
			}
		}
	} else if ( norm === 'F' || norm === 'f' || norm === 'E' || norm === 'e' ) {
		// Frobenius norm: scale * sqrt(sumsq) using zlassq per column
		scale = 0.0;
		sum = 1.0;
		for ( j = 0; j < N; j++ ) {
			// Call zlassq on column j.
			// Column j of A starts at offsetA + j * sa2
			// The stride between rows in interleaved storage is sa1
			// zlassq expects (N, x, stride, offset, scale, sumsq)
			// where stride is in complex elements and offset is byte index
			out = zlassq( M, A, strideA1, offsetA + j * sa2, scale, sum );
			scale = out.scl;
			sum = out.sumsq;
		}
		value = scale * Math.sqrt( sum );
	} else {
		value = 0.0;
	}

	return value;
}


// EXPORTS //

module.exports = zlange;
