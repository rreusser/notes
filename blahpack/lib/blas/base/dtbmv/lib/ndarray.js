

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Perform matrix-vector operation with a triangular band matrix
*
* @param {string} uplo - specifies the operation type
* @param {string} trans - specifies the operation type
* @param {string} diag - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {NonNegativeInteger} K - number of superdiagonals
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} x - output array
* @param {integer} strideX - stride length for `x`
* @param {NonNegativeInteger} offsetX - starting index for `x`
*/
function dtbmv( uplo, trans, diag, N, K, A, strideA1, strideA2, offsetA, x, strideX, offsetX ) { // eslint-disable-line max-len, max-params
	return base( uplo, trans, diag, N, K, A, strideA1, strideA2, offsetA, x, strideX, offsetX ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dtbmv;
