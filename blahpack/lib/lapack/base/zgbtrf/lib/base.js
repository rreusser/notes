

'use strict';

// MAIN //

/**
* Compute LU factorization of a complex banded matrix (blocked)
*
* @private
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {integer} kl - kl
* @param {integer} ku - ku
* @param {Float64Array} AB - input matrix
* @param {integer} strideAB1 - stride of the first dimension of `AB`
* @param {integer} strideAB2 - stride of the second dimension of `AB`
* @param {NonNegativeInteger} offsetAB - starting index for `AB`
* @param {Int32Array} IPIV - output array
* @param {integer} strideIPIV - stride length for `IPIV`
* @param {NonNegativeInteger} offsetIPIV - starting index for `IPIV`
* @returns {integer} status code (0 = success)
*/
function zgbtrf( M, N, kl, ku, AB, strideAB1, strideAB2, offsetAB, IPIV, strideIPIV, offsetIPIV ) { // eslint-disable-line max-len, max-params
	// TODO: implement
	throw new Error( 'not yet implemented' );
}


// EXPORTS //

module.exports = zgbtrf;
