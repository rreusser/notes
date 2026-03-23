

'use strict';

// MAIN //

/**
* Compute a partial factorization of a complex symmetric matrix using Bunch-Kaufman pivoting
*
* @private
* @param {string} uplo - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {integer} nb - nb
* @param {integer} kb - kb
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Int32Array} IPIV - input array
* @param {integer} strideIPIV - stride length for `IPIV`
* @param {NonNegativeInteger} offsetIPIV - starting index for `IPIV`
* @param {Float64Array} W - output matrix
* @param {integer} strideW1 - stride of the first dimension of `W`
* @param {integer} strideW2 - stride of the second dimension of `W`
* @param {NonNegativeInteger} offsetW - starting index for `W`
* @returns {integer} status code (0 = success)
*/
function zlasyf( uplo, N, nb, kb, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV, W, strideW1, strideW2, offsetW ) { // eslint-disable-line max-len, max-params
	// TODO: implement
	throw new Error( 'not yet implemented' );
}


// EXPORTS //

module.exports = zlasyf;
