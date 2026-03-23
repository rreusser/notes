'use strict';

// MAIN //

/**
* Solves one of the systems of equations:
*   A*x = b,  or  A**T*x = b
* where b is overwritten with the solution x, x is an N element vector,
* and A is an N by N unit or non-unit, upper or lower triangular matrix.
*
* @private
* @param {string} uplo - 'U' or 'L'
* @param {string} trans - 'N', 'T', or 'C'
* @param {string} diag - 'U' or 'N'
* @param {NonNegativeInteger} N - order of the matrix
* @param {Float64Array} A - triangular matrix
* @param {integer} strideA1 - stride of the first dimension of A
* @param {integer} strideA2 - stride of the second dimension of A
* @param {NonNegativeInteger} offsetA - starting index for A
* @param {Float64Array} x - input/output vector (b on entry, x on exit)
* @param {integer} strideX - stride for x
* @param {NonNegativeInteger} offsetX - starting index for x
* @returns {Float64Array} `x`
*/
function dtrsv( uplo, trans, diag, N, A, strideA1, strideA2, offsetA, x, strideX, offsetX ) { // eslint-disable-line max-len, max-params
	var nounit;
	var temp;
	var sa1;
	var sa2;
	var ix;
	var jx;
	var ia;
	var i;
	var j;

	if ( N <= 0 ) {
		return x;
	}

	nounit = ( diag === 'N' || diag === 'n' );
	sa1 = strideA1;
	sa2 = strideA2;

	if ( trans === 'N' || trans === 'n' ) {
		// Solve A*x = b
		if ( uplo === 'U' || uplo === 'u' ) {
			// Upper triangular, no transpose: back-substitution from bottom
			jx = offsetX + ( N - 1 ) * strideX;
			for ( j = N - 1; j >= 0; j-- ) {
				if ( x[ jx ] !== 0.0 ) {
					if ( nounit ) {
						x[ jx ] /= A[ offsetA + j * sa1 + j * sa2 ];
					}
					temp = x[ jx ];
					ix = jx - strideX;
					ia = offsetA + ( j - 1 ) * sa1 + j * sa2;
					for ( i = j - 1; i >= 0; i-- ) {
						x[ ix ] -= temp * A[ ia ];
						ix -= strideX;
						ia -= sa1;
					}
				}
				jx -= strideX;
			}
		} else {
			// Lower triangular, no transpose: forward-substitution from top
			jx = offsetX;
			for ( j = 0; j < N; j++ ) {
				if ( x[ jx ] !== 0.0 ) {
					if ( nounit ) {
						x[ jx ] /= A[ offsetA + j * sa1 + j * sa2 ];
					}
					temp = x[ jx ];
					ix = jx + strideX;
					ia = offsetA + ( j + 1 ) * sa1 + j * sa2;
					for ( i = j + 1; i < N; i++ ) {
						x[ ix ] -= temp * A[ ia ];
						ix += strideX;
						ia += sa1;
					}
				}
				jx += strideX;
			}
		}
	} else {
		// Solve A**T*x = b (trans = 'T' or 'C')
		if ( uplo === 'U' || uplo === 'u' ) {
			// Upper triangular, transpose: forward-substitution from top
			jx = offsetX;
			for ( j = 0; j < N; j++ ) {
				temp = x[ jx ];
				ix = offsetX;
				ia = offsetA + j * sa2;
				for ( i = 0; i < j; i++ ) {
					temp -= A[ ia ] * x[ ix ];
					ix += strideX;
					ia += sa1;
				}
				if ( nounit ) {
					temp /= A[ offsetA + j * sa1 + j * sa2 ];
				}
				x[ jx ] = temp;
				jx += strideX;
			}
		} else {
			// Lower triangular, transpose: back-substitution from bottom
			jx = offsetX + ( N - 1 ) * strideX;
			for ( j = N - 1; j >= 0; j-- ) {
				temp = x[ jx ];
				ix = offsetX + ( N - 1 ) * strideX;
				ia = offsetA + ( N - 1 ) * sa1 + j * sa2;
				for ( i = N - 1; i > j; i-- ) {
					temp -= A[ ia ] * x[ ix ];
					ix -= strideX;
					ia -= sa1;
				}
				if ( nounit ) {
					temp /= A[ offsetA + j * sa1 + j * sa2 ];
				}
				x[ jx ] = temp;
				jx -= strideX;
			}
		}
	}
	return x;
}


// EXPORTS //

module.exports = dtrsv;
