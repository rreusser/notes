'use strict';

// MAIN //

/**
* Scans a real matrix for its last non-zero row.
*
* @private
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of A
* @param {integer} strideA2 - stride of the second dimension of A
* @param {NonNegativeInteger} offsetA - starting index for A
* @returns {integer} 0-based index of last non-zero row, or -1 if none
*/
function iladlr( M, N, A, strideA1, strideA2, offsetA ) {
	var result;
	var i;
	var j;

	if ( M === 0 ) {
		return -1;
	}

	// Quick test for the common case where one corner is non-zero.
	if ( A[ offsetA + ( M - 1 ) * strideA1 ] !== 0.0 ||
		A[ offsetA + ( M - 1 ) * strideA1 + ( N - 1 ) * strideA2 ] !== 0.0 ) {
		return M - 1;
	}

	// Scan up each column tracking the last non-zero row seen.
	result = -1;
	for ( j = 0; j < N; j++ ) {
		i = M - 1;
		while ( i >= 0 && A[ offsetA + i * strideA1 + j * strideA2 ] === 0.0 ) {
			i--;
		}
		if ( i > result ) {
			result = i;
		}
	}
	return result;
}


// EXPORTS //

module.exports = iladlr;
