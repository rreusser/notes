

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Compute Cholesky factorization of a symmetric positive definite banded matrix (blocked)
*
* @param {string} uplo - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {integer} kd - kd
* @param {Float64Array} AB - input matrix
* @param {integer} strideAB1 - stride of the first dimension of `AB`
* @param {integer} strideAB2 - stride of the second dimension of `AB`
* @param {NonNegativeInteger} offsetAB - starting index for `AB`
* @returns {integer} status code (0 = success)
*/
function dpbtrf( uplo, N, kd, AB, strideAB1, strideAB2, offsetAB ) { // eslint-disable-line max-len, max-params
	return base( uplo, N, kd, AB, strideAB1, strideAB2, offsetAB ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dpbtrf;
