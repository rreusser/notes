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

// MAIN //

/**
* Perform one of the matrix-matrix operations
*   B := alpha*op(A)*B,  or  B := alpha*B*op(A)
*
* where alpha is a complex scalar, B is an M-by-N matrix, A is a unit
* or non-unit, upper or lower triangular matrix, and op(A) is one of
*   op(A) = A  or  op(A) = A^T  or  op(A) = A^H.
*
* Complex elements are stored as interleaved real/imaginary pairs.
* Strides are in complex-element units.
*
* @private
* @param {string} side - 'L' or 'R'
* @param {string} uplo - 'U' or 'L'
* @param {string} transa - 'N', 'T', or 'C'
* @param {string} diag - 'U' or 'N'
* @param {NonNegativeInteger} M - number of rows of B
* @param {NonNegativeInteger} N - number of columns of B
* @param {Float64Array} alpha - complex scalar [re, im]
* @param {Float64Array} A - triangular matrix (interleaved complex)
* @param {integer} strideA1 - stride of first dim of A (complex elements)
* @param {integer} strideA2 - stride of second dim of A (complex elements)
* @param {NonNegativeInteger} offsetA - starting index for A
* @param {Float64Array} B - matrix (interleaved complex), modified in-place
* @param {integer} strideB1 - stride of first dim of B (complex elements)
* @param {integer} strideB2 - stride of second dim of B (complex elements)
* @param {NonNegativeInteger} offsetB - starting index for B
* @returns {Float64Array} `B`
*/
function ztrmm( side, uplo, transa, diag, M, N, alpha, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB ) { // eslint-disable-line max-len, max-params
	var noconj;
	var nounit;
	var lside;
	var upper;
	var alphaR;
	var alphaI;
	var tempR;
	var tempI;
	var sa1;
	var sa2;
	var sb1;
	var sb2;
	var ia;
	var ib;
	var jb;
	var kb;
	var ar;
	var ai;
	var br;
	var bi;
	var tr;
	var ti;
	var i;
	var j;
	var k;

	if ( M === 0 || N === 0 ) {
		return B;
	}

	lside = ( side === 'L' || side === 'l' );
	upper = ( uplo === 'U' || uplo === 'u' );
	noconj = ( transa === 'T' || transa === 't' );
	nounit = ( diag === 'N' || diag === 'n' );

	alphaR = alpha[ 0 ];
	alphaI = alpha[ 1 ];

	sa1 = strideA1 * 2;
	sa2 = strideA2 * 2;
	sb1 = strideB1 * 2;
	sb2 = strideB2 * 2;

	if ( alphaR === 0.0 && alphaI === 0.0 ) {
		for ( j = 0; j < N; j++ ) {
			for ( i = 0; i < M; i++ ) {
				ib = offsetB + i * sb1 + j * sb2;
				B[ ib ] = 0.0;
				B[ ib + 1 ] = 0.0;
			}
		}
		return B;
	}

	if ( lside ) {
		if ( transa === 'N' || transa === 'n' ) {
			if ( upper ) {
				for ( j = 0; j < N; j++ ) {
					for ( k = 0; k < M; k++ ) {
						kb = offsetB + k * sb1 + j * sb2;
						br = B[ kb ];
						bi = B[ kb + 1 ];
						if ( br !== 0.0 || bi !== 0.0 ) {
							tempR = alphaR * br - alphaI * bi;
							tempI = alphaR * bi + alphaI * br;
							for ( i = 0; i < k; i++ ) {
								ib = offsetB + i * sb1 + j * sb2;
								ia = offsetA + i * sa1 + k * sa2;
								ar = A[ ia ];
								ai = A[ ia + 1 ];
								B[ ib ] += tempR * ar - tempI * ai;
								B[ ib + 1 ] += tempR * ai + tempI * ar;
							}
							if ( nounit ) {
								ia = offsetA + k * sa1 + k * sa2;
								ar = A[ ia ];
								ai = A[ ia + 1 ];
								B[ kb ] = tempR * ar - tempI * ai;
								B[ kb + 1 ] = tempR * ai + tempI * ar;
							} else {
								B[ kb ] = tempR;
								B[ kb + 1 ] = tempI;
							}
						}
					}
				}
			} else {
				for ( j = 0; j < N; j++ ) {
					for ( k = M - 1; k >= 0; k-- ) {
						kb = offsetB + k * sb1 + j * sb2;
						br = B[ kb ];
						bi = B[ kb + 1 ];
						if ( br !== 0.0 || bi !== 0.0 ) {
							tempR = alphaR * br - alphaI * bi;
							tempI = alphaR * bi + alphaI * br;
							B[ kb ] = tempR;
							B[ kb + 1 ] = tempI;
							if ( nounit ) {
								ia = offsetA + k * sa1 + k * sa2;
								ar = A[ ia ];
								ai = A[ ia + 1 ];
								B[ kb ] = tempR * ar - tempI * ai;
								B[ kb + 1 ] = tempR * ai + tempI * ar;
							}
							for ( i = k + 1; i < M; i++ ) {
								ib = offsetB + i * sb1 + j * sb2;
								ia = offsetA + i * sa1 + k * sa2;
								ar = A[ ia ];
								ai = A[ ia + 1 ];
								B[ ib ] += tempR * ar - tempI * ai;
								B[ ib + 1 ] += tempR * ai + tempI * ar;
							}
						}
					}
				}
			}
		} else {
			if ( upper ) {
				for ( j = 0; j < N; j++ ) {
					for ( i = M - 1; i >= 0; i-- ) {
						ib = offsetB + i * sb1 + j * sb2;
						tempR = B[ ib ];
						tempI = B[ ib + 1 ];
						if ( noconj ) {
							if ( nounit ) {
								ia = offsetA + i * sa1 + i * sa2;
								ar = A[ ia ];
								ai = A[ ia + 1 ];
								tr = tempR * ar - tempI * ai;
								ti = tempR * ai + tempI * ar;
								tempR = tr;
								tempI = ti;
							}
							for ( k = 0; k < i; k++ ) {
								ia = offsetA + k * sa1 + i * sa2;
								kb = offsetB + k * sb1 + j * sb2;
								ar = A[ ia ];
								ai = A[ ia + 1 ];
								tempR += ar * B[ kb ] - ai * B[ kb + 1 ];
								tempI += ar * B[ kb + 1 ] + ai * B[ kb ];
							}
						} else {
							if ( nounit ) {
								ia = offsetA + i * sa1 + i * sa2;
								ar = A[ ia ];
								ai = -A[ ia + 1 ];
								tr = tempR * ar - tempI * ai;
								ti = tempR * ai + tempI * ar;
								tempR = tr;
								tempI = ti;
							}
							for ( k = 0; k < i; k++ ) {
								ia = offsetA + k * sa1 + i * sa2;
								kb = offsetB + k * sb1 + j * sb2;
								ar = A[ ia ];
								ai = -A[ ia + 1 ];
								tempR += ar * B[ kb ] - ai * B[ kb + 1 ];
								tempI += ar * B[ kb + 1 ] + ai * B[ kb ];
							}
						}
						B[ ib ] = alphaR * tempR - alphaI * tempI;
						B[ ib + 1 ] = alphaR * tempI + alphaI * tempR;
					}
				}
			} else {
				for ( j = 0; j < N; j++ ) {
					for ( i = 0; i < M; i++ ) {
						ib = offsetB + i * sb1 + j * sb2;
						tempR = B[ ib ];
						tempI = B[ ib + 1 ];
						if ( noconj ) {
							if ( nounit ) {
								ia = offsetA + i * sa1 + i * sa2;
								ar = A[ ia ];
								ai = A[ ia + 1 ];
								tr = tempR * ar - tempI * ai;
								ti = tempR * ai + tempI * ar;
								tempR = tr;
								tempI = ti;
							}
							for ( k = i + 1; k < M; k++ ) {
								ia = offsetA + k * sa1 + i * sa2;
								kb = offsetB + k * sb1 + j * sb2;
								ar = A[ ia ];
								ai = A[ ia + 1 ];
								tempR += ar * B[ kb ] - ai * B[ kb + 1 ];
								tempI += ar * B[ kb + 1 ] + ai * B[ kb ];
							}
						} else {
							if ( nounit ) {
								ia = offsetA + i * sa1 + i * sa2;
								ar = A[ ia ];
								ai = -A[ ia + 1 ];
								tr = tempR * ar - tempI * ai;
								ti = tempR * ai + tempI * ar;
								tempR = tr;
								tempI = ti;
							}
							for ( k = i + 1; k < M; k++ ) {
								ia = offsetA + k * sa1 + i * sa2;
								kb = offsetB + k * sb1 + j * sb2;
								ar = A[ ia ];
								ai = -A[ ia + 1 ];
								tempR += ar * B[ kb ] - ai * B[ kb + 1 ];
								tempI += ar * B[ kb + 1 ] + ai * B[ kb ];
							}
						}
						B[ ib ] = alphaR * tempR - alphaI * tempI;
						B[ ib + 1 ] = alphaR * tempI + alphaI * tempR;
					}
				}
			}
		}
	} else {
		if ( transa === 'N' || transa === 'n' ) {
			if ( upper ) {
				for ( j = N - 1; j >= 0; j-- ) {
					tempR = alphaR;
					tempI = alphaI;
					if ( nounit ) {
						ia = offsetA + j * sa1 + j * sa2;
						ar = A[ ia ];
						ai = A[ ia + 1 ];
						tr = tempR * ar - tempI * ai;
						ti = tempR * ai + tempI * ar;
						tempR = tr;
						tempI = ti;
					}
					for ( i = 0; i < M; i++ ) {
						ib = offsetB + i * sb1 + j * sb2;
						br = B[ ib ];
						bi = B[ ib + 1 ];
						B[ ib ] = tempR * br - tempI * bi;
						B[ ib + 1 ] = tempR * bi + tempI * br;
					}
					for ( k = 0; k < j; k++ ) {
						ia = offsetA + k * sa1 + j * sa2;
						ar = A[ ia ];
						ai = A[ ia + 1 ];
						if ( ar !== 0.0 || ai !== 0.0 ) {
							tr = alphaR * ar - alphaI * ai;
							ti = alphaR * ai + alphaI * ar;
							for ( i = 0; i < M; i++ ) {
								ib = offsetB + i * sb1 + j * sb2;
								kb = offsetB + i * sb1 + k * sb2;
								B[ ib ] += tr * B[ kb ] - ti * B[ kb + 1 ];
								B[ ib + 1 ] += tr * B[ kb + 1 ] + ti * B[ kb ];
							}
						}
					}
				}
			} else {
				for ( j = 0; j < N; j++ ) {
					tempR = alphaR;
					tempI = alphaI;
					if ( nounit ) {
						ia = offsetA + j * sa1 + j * sa2;
						ar = A[ ia ];
						ai = A[ ia + 1 ];
						tr = tempR * ar - tempI * ai;
						ti = tempR * ai + tempI * ar;
						tempR = tr;
						tempI = ti;
					}
					for ( i = 0; i < M; i++ ) {
						ib = offsetB + i * sb1 + j * sb2;
						br = B[ ib ];
						bi = B[ ib + 1 ];
						B[ ib ] = tempR * br - tempI * bi;
						B[ ib + 1 ] = tempR * bi + tempI * br;
					}
					for ( k = j + 1; k < N; k++ ) {
						ia = offsetA + k * sa1 + j * sa2;
						ar = A[ ia ];
						ai = A[ ia + 1 ];
						if ( ar !== 0.0 || ai !== 0.0 ) {
							tr = alphaR * ar - alphaI * ai;
							ti = alphaR * ai + alphaI * ar;
							for ( i = 0; i < M; i++ ) {
								ib = offsetB + i * sb1 + j * sb2;
								kb = offsetB + i * sb1 + k * sb2;
								B[ ib ] += tr * B[ kb ] - ti * B[ kb + 1 ];
								B[ ib + 1 ] += tr * B[ kb + 1 ] + ti * B[ kb ];
							}
						}
					}
				}
			}
		} else {
			if ( upper ) {
				for ( k = 0; k < N; k++ ) {
					for ( j = 0; j < k; j++ ) {
						ia = offsetA + j * sa1 + k * sa2;
						ar = A[ ia ];
						ai = A[ ia + 1 ];
						if ( ar !== 0.0 || ai !== 0.0 ) {
							if ( !noconj ) {
								ai = -ai;
							}
							tr = alphaR * ar - alphaI * ai;
							ti = alphaR * ai + alphaI * ar;
							for ( i = 0; i < M; i++ ) {
								jb = offsetB + i * sb1 + j * sb2;
								kb = offsetB + i * sb1 + k * sb2;
								B[ jb ] += tr * B[ kb ] - ti * B[ kb + 1 ];
								B[ jb + 1 ] += tr * B[ kb + 1 ] + ti * B[ kb ];
							}
						}
					}
					tempR = alphaR;
					tempI = alphaI;
					if ( nounit ) {
						ia = offsetA + k * sa1 + k * sa2;
						ar = A[ ia ];
						ai = A[ ia + 1 ];
						if ( !noconj ) {
							ai = -ai;
						}
						tr = tempR * ar - tempI * ai;
						ti = tempR * ai + tempI * ar;
						tempR = tr;
						tempI = ti;
					}
					if ( tempR !== 1.0 || tempI !== 0.0 ) {
						for ( i = 0; i < M; i++ ) {
							kb = offsetB + i * sb1 + k * sb2;
							br = B[ kb ];
							bi = B[ kb + 1 ];
							B[ kb ] = tempR * br - tempI * bi;
							B[ kb + 1 ] = tempR * bi + tempI * br;
						}
					}
				}
			} else {
				for ( k = N - 1; k >= 0; k-- ) {
					for ( j = k + 1; j < N; j++ ) {
						ia = offsetA + j * sa1 + k * sa2;
						ar = A[ ia ];
						ai = A[ ia + 1 ];
						if ( ar !== 0.0 || ai !== 0.0 ) {
							if ( !noconj ) {
								ai = -ai;
							}
							tr = alphaR * ar - alphaI * ai;
							ti = alphaR * ai + alphaI * ar;
							for ( i = 0; i < M; i++ ) {
								jb = offsetB + i * sb1 + j * sb2;
								kb = offsetB + i * sb1 + k * sb2;
								B[ jb ] += tr * B[ kb ] - ti * B[ kb + 1 ];
								B[ jb + 1 ] += tr * B[ kb + 1 ] + ti * B[ kb ];
							}
						}
					}
					tempR = alphaR;
					tempI = alphaI;
					if ( nounit ) {
						ia = offsetA + k * sa1 + k * sa2;
						ar = A[ ia ];
						ai = A[ ia + 1 ];
						if ( !noconj ) {
							ai = -ai;
						}
						tr = tempR * ar - tempI * ai;
						ti = tempR * ai + tempI * ar;
						tempR = tr;
						tempI = ti;
					}
					if ( tempR !== 1.0 || tempI !== 0.0 ) {
						for ( i = 0; i < M; i++ ) {
							kb = offsetB + i * sb1 + k * sb2;
							br = B[ kb ];
							bi = B[ kb + 1 ];
							B[ kb ] = tempR * br - tempI * bi;
							B[ kb + 1 ] = tempR * bi + tempI * br;
						}
					}
				}
			}
		}
	}
	return B;
}


// EXPORTS //

module.exports = ztrmm;
