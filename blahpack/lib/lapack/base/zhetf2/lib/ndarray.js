

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Complex Hermitian indefinite factorization (unblocked Bunch-Kaufman)
*
* @param {string} uplo - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Int32Array} IPIV - output array
* @param {integer} strideIPIV - stride length for `IPIV`
* @param {NonNegativeInteger} offsetIPIV - starting index for `IPIV`
* @returns {integer} status code (0 = success)
*/
function zhetf2( uplo, N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV ) { // eslint-disable-line max-len, max-params
	return base( uplo, N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zhetf2;
