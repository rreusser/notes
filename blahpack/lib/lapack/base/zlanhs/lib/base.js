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
var cmplx = require( '../../../../cmplx.js' );


// MAIN //

/**
* Returns the value of the one norm, Frobenius norm, infinity norm, or
* max absolute value of an upper Hessenberg complex matrix.
*
* @private
* @param {string} norm - 'M' (max), '1'/'O' (one-norm), 'I' (inf-norm), 'F'/'E' (Frobenius)
* @param {NonNegativeInteger} N - order of the matrix
* @param {Float64Array} A - upper Hessenberg matrix (interleaved complex)
* @param {integer} strideA1 - stride of the first dimension of A
* @param {integer} strideA2 - stride of the second dimension of A
* @param {NonNegativeInteger} offsetA - starting index for A
* @param {Float64Array} WORK - workspace (length >= N, used for inf-norm only)
* @param {integer} strideWORK - stride for WORK
* @param {NonNegativeInteger} offsetWORK - starting index for WORK
* @returns {number} matrix norm value
*/
function zlanhs( norm, N, A, strideA1, strideA2, offsetA, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	var value;
	var scale;
	var sum;
	var sa1;
	var sa2;
	var lim;
	var aij;
	var ar;
	var ai;
	var wi;
	var i;
	var j;

	sa1 = strideA1;
	sa2 = strideA2;

	if ( N === 0 ) {
		return 0.0;
	}

	var n = norm.charAt( 0 ).toUpperCase();

	if ( n === 'M' ) {
		// Max absolute value
		value = 0.0;
		for ( j = 0; j < N; j++ ) {
			lim = Math.min( N, j + 2 ); // upper Hessenberg: rows 0..min(N-1, j+1)
			aij = offsetA + 2 * j * sa2;
			for ( i = 0; i < lim; i++ ) {
				// |A(i,j)| — overflow-safe complex absolute value
				sum = cmplx.abs( A.subarray( aij, aij + 2 ) );
				if ( value < sum || sum !== sum ) {
					value = sum;
				}
				aij += 2 * sa1;
			}
		}
	} else if ( n === 'O' || n === '1' ) {
		// One-norm (max column sum of absolute values)
		value = 0.0;
		for ( j = 0; j < N; j++ ) {
			sum = 0.0;
			lim = Math.min( N, j + 2 );
			aij = offsetA + 2 * j * sa2;
			for ( i = 0; i < lim; i++ ) {
				// |A(i,j)|
				sum += cmplx.abs( A.subarray( aij, aij + 2 ) );
				aij += 2 * sa1;
			}
			if ( value < sum || sum !== sum ) {
				value = sum;
			}
		}
	} else if ( n === 'I' ) {
		// Infinity-norm (max row sum of absolute values)
		for ( i = 0; i < N; i++ ) {
			WORK[ offsetWORK + i * strideWORK ] = 0.0;
		}
		for ( j = 0; j < N; j++ ) {
			lim = Math.min( N, j + 2 );
			aij = offsetA + 2 * j * sa2;
			wi = offsetWORK;
			for ( i = 0; i < lim; i++ ) {
				// |A(i,j)|
				WORK[ wi ] += cmplx.abs( A.subarray( aij, aij + 2 ) );
				aij += 2 * sa1;
				wi += strideWORK;
			}
		}
		value = 0.0;
		for ( i = 0; i < N; i++ ) {
			sum = WORK[ offsetWORK + i * strideWORK ];
			if ( value < sum || sum !== sum ) {
				value = sum;
			}
		}
	} else if ( n === 'F' || n === 'E' ) {
		// Frobenius norm
		scale = 0.0;
		sum = 1.0;
		for ( j = 0; j < N; j++ ) {
			lim = Math.min( N, j + 2 );
			var result = zlassq( lim, A, sa1, offsetA + 2 * j * sa2, scale, sum );
			scale = result.scl;
			sum = result.sumsq;
		}
		value = scale * Math.sqrt( sum );
	} else {
		value = 0.0;
	}

	return value;
}


// EXPORTS //

module.exports = zlanhs;
