

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Solve a Hermitian positive definite banded system using Cholesky factorization
*
* @param {string} uplo - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {integer} kd - kd
* @param {integer} nrhs - nrhs
* @param {Float64Array} AB - input matrix
* @param {integer} strideAB1 - stride of the first dimension of `AB`
* @param {integer} strideAB2 - stride of the second dimension of `AB`
* @param {NonNegativeInteger} offsetAB - starting index for `AB`
* @param {Float64Array} B - output matrix
* @param {integer} strideB1 - stride of the first dimension of `B`
* @param {integer} strideB2 - stride of the second dimension of `B`
* @param {NonNegativeInteger} offsetB - starting index for `B`
* @returns {integer} status code (0 = success)
*/
function zpbtrs( uplo, N, kd, nrhs, AB, strideAB1, strideAB2, offsetAB, B, strideB1, strideB2, offsetB ) { // eslint-disable-line max-len, max-params
	return base( uplo, N, kd, nrhs, AB, strideAB1, strideAB2, offsetAB, B, strideB1, strideB2, offsetB ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zpbtrs;
