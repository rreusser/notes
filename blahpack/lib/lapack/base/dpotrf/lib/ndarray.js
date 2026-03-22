

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Compute the Cholesky factorization of a real symmetric positive definite matrix.
*
* @param {string} uplo - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @returns {integer} status code (0 = success)
*/
function dpotrf( uplo, N, A, strideA1, strideA2, offsetA ) {
	return base( uplo, N, A, strideA1, strideA2, offsetA );
}


// EXPORTS //

module.exports = dpotrf;
