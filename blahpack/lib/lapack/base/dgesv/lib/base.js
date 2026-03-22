

'use strict';

// MAIN //

/**
* Compute the solution to a real system of linear equations A*X=B.
*
* @private
* @param {NonNegativeInteger} N - number of columns
* @param {integer} nrhs - nrhs
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Int32Array} IPIV - input array
* @param {integer} strideIPIV - stride length for `IPIV`
* @param {NonNegativeInteger} offsetIPIV - starting index for `IPIV`
* @param {Float64Array} B - output matrix
* @param {integer} strideB1 - stride of the first dimension of `B`
* @param {integer} strideB2 - stride of the second dimension of `B`
* @param {NonNegativeInteger} offsetB - starting index for `B`
* @returns {integer} status code (0 = success)
*/
function dgesv( N, nrhs, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV, B, strideB1, strideB2, offsetB ) { // eslint-disable-line max-len, max-params
	// TODO: implement
	throw new Error( 'not yet implemented' );
}


// EXPORTS //

module.exports = dgesv;
