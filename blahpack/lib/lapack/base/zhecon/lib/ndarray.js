

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Estimate the reciprocal condition number of a Hermitian indefinite matrix
*
* @param {string} uplo - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Int32Array} IPIV - input array
* @param {integer} strideIPIV - stride length for `IPIV`
* @param {NonNegativeInteger} offsetIPIV - starting index for `IPIV`
* @param {number} anorm - anorm
* @param {number} rcond - rcond
* @param {Float64Array} WORK - output array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @returns {integer} status code (0 = success)
*/
function zhecon( uplo, N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV, anorm, rcond, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	return base( uplo, N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV, anorm, rcond, WORK, strideWORK, offsetWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zhecon;
