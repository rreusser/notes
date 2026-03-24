

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Converts a symmetric matrix factored by dsytrf to standard L*D*L^T form and vice versa
*
* @param {string} uplo - specifies the operation type
* @param {string} way - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Int32Array} IPIV - input array
* @param {integer} strideIPIV - stride length for `IPIV`
* @param {NonNegativeInteger} offsetIPIV - starting index for `IPIV`
* @param {Float64Array} e - output array
* @param {integer} strideE - stride length for `e`
* @param {NonNegativeInteger} offsetE - starting index for `e`
* @returns {integer} status code (0 = success)
*/
function dsyconv( uplo, way, N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV, e, strideE, offsetE ) { // eslint-disable-line max-len, max-params
	return base( uplo, way, N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV, e, strideE, offsetE ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dsyconv;
