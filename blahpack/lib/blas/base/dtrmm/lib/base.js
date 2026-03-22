'use strict';

// MAIN //

/**
* Performs one of the matrix-matrix operations
*   B := alpha*op(A)*B,  or  B := alpha*B*op(A)
*
* where alpha is a scalar, B is an M-by-N matrix, A is a unit or
* non-unit, upper or lower triangular matrix, and op(A) is A or A**T.
*
* @private
* @param {string} side - 'L' or 'R'
* @param {string} uplo - 'U' or 'L'
* @param {string} transa - 'N' or 'T'
* @param {string} diag - 'U' or 'N'
* @param {NonNegativeInteger} M - number of rows of B
* @param {NonNegativeInteger} N - number of columns of B
* @param {number} alpha - scalar multiplier
* @param {Float64Array} A - triangular matrix
* @param {integer} strideA1 - stride of first dim of A
* @param {integer} strideA2 - stride of second dim of A
* @param {NonNegativeInteger} offsetA - starting index for A
* @param {Float64Array} B - matrix, modified in-place
* @param {integer} strideB1 - stride of first dim of B
* @param {integer} strideB2 - stride of second dim of B
* @param {NonNegativeInteger} offsetB - starting index for B
* @returns {Float64Array} B
*/
function dtrmm( side, uplo, transa, diag, M, N, alpha, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB ) { // eslint-disable-line max-len, max-params
	var nounit;
	var lside;
	var upper;
	var temp;
	var i;
	var j;
	var k;

	if ( M === 0 || N === 0 ) {
		return B;
	}

	lside = ( side === 'L' || side === 'l' );
	upper = ( uplo === 'U' || uplo === 'u' );
	nounit = ( diag === 'N' || diag === 'n' );

	if ( alpha === 0.0 ) {
		for ( j = 0; j < N; j++ ) {
			for ( i = 0; i < M; i++ ) {
				B[ offsetB + i * strideB1 + j * strideB2 ] = 0.0;
			}
		}
		return B;
	}

	if ( lside ) {
		if ( transa === 'N' || transa === 'n' ) {
			// B := alpha * A * B
			if ( upper ) {
				for ( j = 0; j < N; j++ ) {
					for ( k = 0; k < M; k++ ) {
						temp = alpha * B[ offsetB + k * strideB1 + j * strideB2 ];
						if ( temp !== 0.0 ) {
							for ( i = 0; i < k; i++ ) {
								B[ offsetB + i * strideB1 + j * strideB2 ] += temp * A[ offsetA + i * strideA1 + k * strideA2 ];
							}
							if ( nounit ) {
								temp *= A[ offsetA + k * strideA1 + k * strideA2 ];
							}
						}
						B[ offsetB + k * strideB1 + j * strideB2 ] = temp;
					}
				}
			} else {
				for ( j = 0; j < N; j++ ) {
					for ( k = M - 1; k >= 0; k-- ) {
						temp = alpha * B[ offsetB + k * strideB1 + j * strideB2 ];
						if ( temp !== 0.0 ) {
							if ( nounit ) {
								temp *= A[ offsetA + k * strideA1 + k * strideA2 ];
							}
							B[ offsetB + k * strideB1 + j * strideB2 ] = temp;
							for ( i = k + 1; i < M; i++ ) {
								B[ offsetB + i * strideB1 + j * strideB2 ] += temp * A[ offsetA + i * strideA1 + k * strideA2 ];
							}
						} else {
							B[ offsetB + k * strideB1 + j * strideB2 ] = 0.0;
						}
					}
				}
			}
		} else {
			// B := alpha * A**T * B
			if ( upper ) {
				for ( j = 0; j < N; j++ ) {
					for ( i = M - 1; i >= 0; i-- ) {
						temp = B[ offsetB + i * strideB1 + j * strideB2 ];
						if ( nounit ) {
							temp *= A[ offsetA + i * strideA1 + i * strideA2 ];
						}
						for ( k = 0; k < i; k++ ) {
							temp += A[ offsetA + k * strideA1 + i * strideA2 ] * B[ offsetB + k * strideB1 + j * strideB2 ];
						}
						B[ offsetB + i * strideB1 + j * strideB2 ] = alpha * temp;
					}
				}
			} else {
				for ( j = 0; j < N; j++ ) {
					for ( i = 0; i < M; i++ ) {
						temp = B[ offsetB + i * strideB1 + j * strideB2 ];
						if ( nounit ) {
							temp *= A[ offsetA + i * strideA1 + i * strideA2 ];
						}
						for ( k = i + 1; k < M; k++ ) {
							temp += A[ offsetA + k * strideA1 + i * strideA2 ] * B[ offsetB + k * strideB1 + j * strideB2 ];
						}
						B[ offsetB + i * strideB1 + j * strideB2 ] = alpha * temp;
					}
				}
			}
		}
	} else {
		// Right side: B := alpha * B * op(A)
		if ( transa === 'N' || transa === 'n' ) {
			if ( upper ) {
				for ( j = N - 1; j >= 0; j-- ) {
					temp = alpha;
					if ( nounit ) {
						temp *= A[ offsetA + j * strideA1 + j * strideA2 ];
					}
					for ( i = 0; i < M; i++ ) {
						B[ offsetB + i * strideB1 + j * strideB2 ] *= temp;
					}
					for ( k = 0; k < j; k++ ) {
						if ( A[ offsetA + k * strideA1 + j * strideA2 ] !== 0.0 ) {
							temp = alpha * A[ offsetA + k * strideA1 + j * strideA2 ];
							for ( i = 0; i < M; i++ ) {
								B[ offsetB + i * strideB1 + j * strideB2 ] += temp * B[ offsetB + i * strideB1 + k * strideB2 ];
							}
						}
					}
				}
			} else {
				for ( j = 0; j < N; j++ ) {
					temp = alpha;
					if ( nounit ) {
						temp *= A[ offsetA + j * strideA1 + j * strideA2 ];
					}
					for ( i = 0; i < M; i++ ) {
						B[ offsetB + i * strideB1 + j * strideB2 ] *= temp;
					}
					for ( k = j + 1; k < N; k++ ) {
						if ( A[ offsetA + k * strideA1 + j * strideA2 ] !== 0.0 ) {
							temp = alpha * A[ offsetA + k * strideA1 + j * strideA2 ];
							for ( i = 0; i < M; i++ ) {
								B[ offsetB + i * strideB1 + j * strideB2 ] += temp * B[ offsetB + i * strideB1 + k * strideB2 ];
							}
						}
					}
				}
			}
		} else {
			// B := alpha * B * A**T
			if ( upper ) {
				for ( k = 0; k < N; k++ ) {
					for ( j = 0; j < k; j++ ) {
						if ( A[ offsetA + j * strideA1 + k * strideA2 ] !== 0.0 ) {
							temp = alpha * A[ offsetA + j * strideA1 + k * strideA2 ];
							for ( i = 0; i < M; i++ ) {
								B[ offsetB + i * strideB1 + j * strideB2 ] += temp * B[ offsetB + i * strideB1 + k * strideB2 ];
							}
						}
					}
					temp = alpha;
					if ( nounit ) {
						temp *= A[ offsetA + k * strideA1 + k * strideA2 ];
					}
					if ( temp !== 1.0 ) {
						for ( i = 0; i < M; i++ ) {
							B[ offsetB + i * strideB1 + k * strideB2 ] *= temp;
						}
					}
				}
			} else {
				for ( k = N - 1; k >= 0; k-- ) {
					for ( j = k + 1; j < N; j++ ) {
						if ( A[ offsetA + j * strideA1 + k * strideA2 ] !== 0.0 ) {
							temp = alpha * A[ offsetA + j * strideA1 + k * strideA2 ];
							for ( i = 0; i < M; i++ ) {
								B[ offsetB + i * strideB1 + j * strideB2 ] += temp * B[ offsetB + i * strideB1 + k * strideB2 ];
							}
						}
					}
					temp = alpha;
					if ( nounit ) {
						temp *= A[ offsetA + k * strideA1 + k * strideA2 ];
					}
					if ( temp !== 1.0 ) {
						for ( i = 0; i < M; i++ ) {
							B[ offsetB + i * strideB1 + k * strideB2 ] *= temp;
						}
					}
				}
			}
		}
	}
	return B;
}


// EXPORTS //

module.exports = dtrmm;
