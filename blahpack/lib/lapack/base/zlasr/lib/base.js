/**
* @license Apache-2.0
*
* Copyright (c) 2025 Ricky Reusser.
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

// MAIN //

/**
* Applies a sequence of real plane rotations to a complex general matrix.
*
* When SIDE = 'L', the transformation takes the form:
*   A := P * A
* When SIDE = 'R', the transformation takes the form:
*   A := A * P^T
*
* where P is an orthogonal matrix consisting of a sequence of z plane
* rotations, with z = M-1 when SIDE = 'L' and z = N-1 when SIDE = 'R',
* and P^T is the transpose of P.
*
* Complex elements of A are stored as interleaved real/imaginary pairs in a
* Float64Array. Element (i, j) has real part at
* `offsetA + i*strideA1 + j*strideA2` and imaginary part at
* `offsetA + i*strideA1 + j*strideA2 + 1`.
*
* For complex matrices, strideA1 and strideA2 are in units of doubles.
* For column-major with LDA rows: strideA1 = 2, strideA2 = 2*LDA.
*
* C and S are real arrays with simple stride/offset.
*
* @private
* @param {string} side - 'L' to apply from the left, 'R' from the right
* @param {string} pivot - 'V' for variable, 'T' for top, 'B' for bottom
* @param {string} direct - 'F' for forward, 'B' for backward
* @param {NonNegativeInteger} M - number of rows of A
* @param {NonNegativeInteger} N - number of columns of A
* @param {Float64Array} c - array of cosines (real)
* @param {integer} strideC - stride for `c`
* @param {NonNegativeInteger} offsetC - starting index for `c`
* @param {Float64Array} s - array of sines (real)
* @param {integer} strideS - stride for `s`
* @param {NonNegativeInteger} offsetS - starting index for `s`
* @param {Float64Array} A - input/output matrix (interleaved complex)
* @param {integer} strideA1 - stride of the first dimension of A (in doubles)
* @param {integer} strideA2 - stride of the second dimension of A (in doubles)
* @param {NonNegativeInteger} offsetA - starting index for A
* @returns {Float64Array} A
*/
function zlasr( side, pivot, direct, M, N, c, strideC, offsetC, s, strideS, offsetS, A, strideA1, strideA2, offsetA ) { // eslint-disable-line max-len, max-params
	var ctemp;
	var stemp;
	var tempRe;
	var tempIm;
	var aRe;
	var aIm;
	var sa1;
	var sa2;
	var idx1;
	var idx2;
	var i;
	var j;

	if ( M === 0 || N === 0 ) {
		return A;
	}

	sa1 = strideA1;
	sa2 = strideA2;

	if ( side === 'L' || side === 'l' ) {
		// Apply rotations from the left (act on rows)
		if ( pivot === 'V' || pivot === 'v' ) {
			if ( direct === 'F' || direct === 'f' ) {
				// Variable pivot, forward direction
				for ( j = 0; j < M - 1; j++ ) {
					ctemp = c[ offsetC + j * strideC ];
					stemp = s[ offsetS + j * strideS ];
					if ( ctemp !== 1.0 || stemp !== 0.0 ) {
						for ( i = 0; i < N; i++ ) {
							// TEMP = A( J+1, I )
							idx1 = offsetA + ( j + 1 ) * sa1 + i * sa2;
							idx2 = offsetA + j * sa1 + i * sa2;
							tempRe = A[ idx1 ];
							tempIm = A[ idx1 + 1 ];
							aRe = A[ idx2 ];
							aIm = A[ idx2 + 1 ];
							// A( J+1, I ) = CTEMP*TEMP - STEMP*A( J, I )
							A[ idx1 ] = ctemp * tempRe - stemp * aRe;
							A[ idx1 + 1 ] = ctemp * tempIm - stemp * aIm;
							// A( J, I ) = STEMP*TEMP + CTEMP*A( J, I )
							A[ idx2 ] = stemp * tempRe + ctemp * aRe;
							A[ idx2 + 1 ] = stemp * tempIm + ctemp * aIm;
						}
					}
				}
			} else {
				// Variable pivot, backward direction
				for ( j = M - 2; j >= 0; j-- ) {
					ctemp = c[ offsetC + j * strideC ];
					stemp = s[ offsetS + j * strideS ];
					if ( ctemp !== 1.0 || stemp !== 0.0 ) {
						for ( i = 0; i < N; i++ ) {
							idx1 = offsetA + ( j + 1 ) * sa1 + i * sa2;
							idx2 = offsetA + j * sa1 + i * sa2;
							tempRe = A[ idx1 ];
							tempIm = A[ idx1 + 1 ];
							aRe = A[ idx2 ];
							aIm = A[ idx2 + 1 ];
							A[ idx1 ] = ctemp * tempRe - stemp * aRe;
							A[ idx1 + 1 ] = ctemp * tempIm - stemp * aIm;
							A[ idx2 ] = stemp * tempRe + ctemp * aRe;
							A[ idx2 + 1 ] = stemp * tempIm + ctemp * aIm;
						}
					}
				}
			}
		} else if ( pivot === 'T' || pivot === 't' ) {
			if ( direct === 'F' || direct === 'f' ) {
				// Top pivot, forward direction
				for ( j = 1; j < M; j++ ) {
					ctemp = c[ offsetC + ( j - 1 ) * strideC ];
					stemp = s[ offsetS + ( j - 1 ) * strideS ];
					if ( ctemp !== 1.0 || stemp !== 0.0 ) {
						for ( i = 0; i < N; i++ ) {
							// TEMP = A( J, I )
							idx1 = offsetA + j * sa1 + i * sa2;
							idx2 = offsetA + i * sa2; // row 0
							tempRe = A[ idx1 ];
							tempIm = A[ idx1 + 1 ];
							aRe = A[ idx2 ];
							aIm = A[ idx2 + 1 ];
							// A( J, I ) = CTEMP*TEMP - STEMP*A( 1, I )
							A[ idx1 ] = ctemp * tempRe - stemp * aRe;
							A[ idx1 + 1 ] = ctemp * tempIm - stemp * aIm;
							// A( 1, I ) = STEMP*TEMP + CTEMP*A( 1, I )
							A[ idx2 ] = stemp * tempRe + ctemp * aRe;
							A[ idx2 + 1 ] = stemp * tempIm + ctemp * aIm;
						}
					}
				}
			} else {
				// Top pivot, backward direction
				for ( j = M - 1; j >= 1; j-- ) {
					ctemp = c[ offsetC + ( j - 1 ) * strideC ];
					stemp = s[ offsetS + ( j - 1 ) * strideS ];
					if ( ctemp !== 1.0 || stemp !== 0.0 ) {
						for ( i = 0; i < N; i++ ) {
							idx1 = offsetA + j * sa1 + i * sa2;
							idx2 = offsetA + i * sa2; // row 0
							tempRe = A[ idx1 ];
							tempIm = A[ idx1 + 1 ];
							aRe = A[ idx2 ];
							aIm = A[ idx2 + 1 ];
							A[ idx1 ] = ctemp * tempRe - stemp * aRe;
							A[ idx1 + 1 ] = ctemp * tempIm - stemp * aIm;
							A[ idx2 ] = stemp * tempRe + ctemp * aRe;
							A[ idx2 + 1 ] = stemp * tempIm + ctemp * aIm;
						}
					}
				}
			}
		} else if ( pivot === 'B' || pivot === 'b' ) {
			if ( direct === 'F' || direct === 'f' ) {
				// Bottom pivot, forward direction
				for ( j = 0; j < M - 1; j++ ) {
					ctemp = c[ offsetC + j * strideC ];
					stemp = s[ offsetS + j * strideS ];
					if ( ctemp !== 1.0 || stemp !== 0.0 ) {
						for ( i = 0; i < N; i++ ) {
							// TEMP = A( J, I )
							idx1 = offsetA + j * sa1 + i * sa2;
							idx2 = offsetA + ( M - 1 ) * sa1 + i * sa2; // last row
							tempRe = A[ idx1 ];
							tempIm = A[ idx1 + 1 ];
							aRe = A[ idx2 ];
							aIm = A[ idx2 + 1 ];
							// A( J, I ) = STEMP*A( M, I ) + CTEMP*TEMP
							A[ idx1 ] = stemp * aRe + ctemp * tempRe;
							A[ idx1 + 1 ] = stemp * aIm + ctemp * tempIm;
							// A( M, I ) = CTEMP*A( M, I ) - STEMP*TEMP
							A[ idx2 ] = ctemp * aRe - stemp * tempRe;
							A[ idx2 + 1 ] = ctemp * aIm - stemp * tempIm;
						}
					}
				}
			} else {
				// Bottom pivot, backward direction
				for ( j = M - 2; j >= 0; j-- ) {
					ctemp = c[ offsetC + j * strideC ];
					stemp = s[ offsetS + j * strideS ];
					if ( ctemp !== 1.0 || stemp !== 0.0 ) {
						for ( i = 0; i < N; i++ ) {
							idx1 = offsetA + j * sa1 + i * sa2;
							idx2 = offsetA + ( M - 1 ) * sa1 + i * sa2;
							tempRe = A[ idx1 ];
							tempIm = A[ idx1 + 1 ];
							aRe = A[ idx2 ];
							aIm = A[ idx2 + 1 ];
							A[ idx1 ] = stemp * aRe + ctemp * tempRe;
							A[ idx1 + 1 ] = stemp * aIm + ctemp * tempIm;
							A[ idx2 ] = ctemp * aRe - stemp * tempRe;
							A[ idx2 + 1 ] = ctemp * aIm - stemp * tempIm;
						}
					}
				}
			}
		}
	} else if ( side === 'R' || side === 'r' ) {
		// Apply rotations from the right (act on columns)
		if ( pivot === 'V' || pivot === 'v' ) {
			if ( direct === 'F' || direct === 'f' ) {
				// Variable pivot, forward direction
				for ( j = 0; j < N - 1; j++ ) {
					ctemp = c[ offsetC + j * strideC ];
					stemp = s[ offsetS + j * strideS ];
					if ( ctemp !== 1.0 || stemp !== 0.0 ) {
						for ( i = 0; i < M; i++ ) {
							// TEMP = A( I, J+1 )
							idx1 = offsetA + i * sa1 + ( j + 1 ) * sa2;
							idx2 = offsetA + i * sa1 + j * sa2;
							tempRe = A[ idx1 ];
							tempIm = A[ idx1 + 1 ];
							aRe = A[ idx2 ];
							aIm = A[ idx2 + 1 ];
							// A( I, J+1 ) = CTEMP*TEMP - STEMP*A( I, J )
							A[ idx1 ] = ctemp * tempRe - stemp * aRe;
							A[ idx1 + 1 ] = ctemp * tempIm - stemp * aIm;
							// A( I, J ) = STEMP*TEMP + CTEMP*A( I, J )
							A[ idx2 ] = stemp * tempRe + ctemp * aRe;
							A[ idx2 + 1 ] = stemp * tempIm + ctemp * aIm;
						}
					}
				}
			} else {
				// Variable pivot, backward direction
				for ( j = N - 2; j >= 0; j-- ) {
					ctemp = c[ offsetC + j * strideC ];
					stemp = s[ offsetS + j * strideS ];
					if ( ctemp !== 1.0 || stemp !== 0.0 ) {
						for ( i = 0; i < M; i++ ) {
							idx1 = offsetA + i * sa1 + ( j + 1 ) * sa2;
							idx2 = offsetA + i * sa1 + j * sa2;
							tempRe = A[ idx1 ];
							tempIm = A[ idx1 + 1 ];
							aRe = A[ idx2 ];
							aIm = A[ idx2 + 1 ];
							A[ idx1 ] = ctemp * tempRe - stemp * aRe;
							A[ idx1 + 1 ] = ctemp * tempIm - stemp * aIm;
							A[ idx2 ] = stemp * tempRe + ctemp * aRe;
							A[ idx2 + 1 ] = stemp * tempIm + ctemp * aIm;
						}
					}
				}
			}
		} else if ( pivot === 'T' || pivot === 't' ) {
			if ( direct === 'F' || direct === 'f' ) {
				// Top pivot, forward direction
				for ( j = 1; j < N; j++ ) {
					ctemp = c[ offsetC + ( j - 1 ) * strideC ];
					stemp = s[ offsetS + ( j - 1 ) * strideS ];
					if ( ctemp !== 1.0 || stemp !== 0.0 ) {
						for ( i = 0; i < M; i++ ) {
							// TEMP = A( I, J )
							idx1 = offsetA + i * sa1 + j * sa2;
							idx2 = offsetA + i * sa1; // col 0
							tempRe = A[ idx1 ];
							tempIm = A[ idx1 + 1 ];
							aRe = A[ idx2 ];
							aIm = A[ idx2 + 1 ];
							// A( I, J ) = CTEMP*TEMP - STEMP*A( I, 1 )
							A[ idx1 ] = ctemp * tempRe - stemp * aRe;
							A[ idx1 + 1 ] = ctemp * tempIm - stemp * aIm;
							// A( I, 1 ) = STEMP*TEMP + CTEMP*A( I, 1 )
							A[ idx2 ] = stemp * tempRe + ctemp * aRe;
							A[ idx2 + 1 ] = stemp * tempIm + ctemp * aIm;
						}
					}
				}
			} else {
				// Top pivot, backward direction
				for ( j = N - 1; j >= 1; j-- ) {
					ctemp = c[ offsetC + ( j - 1 ) * strideC ];
					stemp = s[ offsetS + ( j - 1 ) * strideS ];
					if ( ctemp !== 1.0 || stemp !== 0.0 ) {
						for ( i = 0; i < M; i++ ) {
							idx1 = offsetA + i * sa1 + j * sa2;
							idx2 = offsetA + i * sa1; // col 0
							tempRe = A[ idx1 ];
							tempIm = A[ idx1 + 1 ];
							aRe = A[ idx2 ];
							aIm = A[ idx2 + 1 ];
							A[ idx1 ] = ctemp * tempRe - stemp * aRe;
							A[ idx1 + 1 ] = ctemp * tempIm - stemp * aIm;
							A[ idx2 ] = stemp * tempRe + ctemp * aRe;
							A[ idx2 + 1 ] = stemp * tempIm + ctemp * aIm;
						}
					}
				}
			}
		} else if ( pivot === 'B' || pivot === 'b' ) {
			if ( direct === 'F' || direct === 'f' ) {
				// Bottom pivot, forward direction
				for ( j = 0; j < N - 1; j++ ) {
					ctemp = c[ offsetC + j * strideC ];
					stemp = s[ offsetS + j * strideS ];
					if ( ctemp !== 1.0 || stemp !== 0.0 ) {
						for ( i = 0; i < M; i++ ) {
							// TEMP = A( I, J )
							idx1 = offsetA + i * sa1 + j * sa2;
							idx2 = offsetA + i * sa1 + ( N - 1 ) * sa2; // last col
							tempRe = A[ idx1 ];
							tempIm = A[ idx1 + 1 ];
							aRe = A[ idx2 ];
							aIm = A[ idx2 + 1 ];
							// A( I, J ) = STEMP*A( I, N ) + CTEMP*TEMP
							A[ idx1 ] = stemp * aRe + ctemp * tempRe;
							A[ idx1 + 1 ] = stemp * aIm + ctemp * tempIm;
							// A( I, N ) = CTEMP*A( I, N ) - STEMP*TEMP
							A[ idx2 ] = ctemp * aRe - stemp * tempRe;
							A[ idx2 + 1 ] = ctemp * aIm - stemp * tempIm;
						}
					}
				}
			} else {
				// Bottom pivot, backward direction
				for ( j = N - 2; j >= 0; j-- ) {
					ctemp = c[ offsetC + j * strideC ];
					stemp = s[ offsetS + j * strideS ];
					if ( ctemp !== 1.0 || stemp !== 0.0 ) {
						for ( i = 0; i < M; i++ ) {
							idx1 = offsetA + i * sa1 + j * sa2;
							idx2 = offsetA + i * sa1 + ( N - 1 ) * sa2;
							tempRe = A[ idx1 ];
							tempIm = A[ idx1 + 1 ];
							aRe = A[ idx2 ];
							aIm = A[ idx2 + 1 ];
							A[ idx1 ] = stemp * aRe + ctemp * tempRe;
							A[ idx1 + 1 ] = stemp * aIm + ctemp * tempIm;
							A[ idx2 ] = ctemp * aRe - stemp * tempRe;
							A[ idx2 + 1 ] = ctemp * aIm - stemp * tempIm;
						}
					}
				}
			}
		}
	}
	return A;
}


// EXPORTS //

module.exports = zlasr;
