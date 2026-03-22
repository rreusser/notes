'use strict';

// MAIN //

/**
* Copy all or part of a complex matrix A to another complex matrix B.
*
* Complex elements are stored as interleaved real/imaginary pairs.
* Element (i, j) has real part at offset + i*stride1 + j*stride2
* and imaginary part at offset + i*stride1 + j*stride2 + 1.
*
* @private
* @param {string} uplo - 'U' for upper, 'L' for lower, otherwise full
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - source matrix (interleaved complex)
* @param {integer} strideA1 - first dimension stride of A
* @param {integer} strideA2 - second dimension stride of A
* @param {NonNegativeInteger} offsetA - starting index for A
* @param {Float64Array} B - destination matrix (interleaved complex)
* @param {integer} strideB1 - first dimension stride of B
* @param {integer} strideB2 - second dimension stride of B
* @param {NonNegativeInteger} offsetB - starting index for B
*/
function zlacpy( uplo, M, N, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB ) { // eslint-disable-line max-len, max-params
	var idxA;
	var idxB;
	var i;
	var j;

	if ( uplo === 'U' || uplo === 'u' ) {
		for ( j = 0; j < N; j++ ) {
			for ( i = 0; i <= j && i < M; i++ ) {
				idxA = offsetA + i * strideA1 + j * strideA2;
				idxB = offsetB + i * strideB1 + j * strideB2;
				B[ idxB ] = A[ idxA ];
				B[ idxB + 1 ] = A[ idxA + 1 ];
			}
		}
	} else if ( uplo === 'L' || uplo === 'l' ) {
		for ( j = 0; j < N; j++ ) {
			for ( i = j; i < M; i++ ) {
				idxA = offsetA + i * strideA1 + j * strideA2;
				idxB = offsetB + i * strideB1 + j * strideB2;
				B[ idxB ] = A[ idxA ];
				B[ idxB + 1 ] = A[ idxA + 1 ];
			}
		}
	} else {
		for ( j = 0; j < N; j++ ) {
			for ( i = 0; i < M; i++ ) {
				idxA = offsetA + i * strideA1 + j * strideA2;
				idxB = offsetB + i * strideB1 + j * strideB2;
				B[ idxB ] = A[ idxA ];
				B[ idxB + 1 ] = A[ idxA + 1 ];
			}
		}
	}
}


// EXPORTS //

module.exports = zlacpy;
