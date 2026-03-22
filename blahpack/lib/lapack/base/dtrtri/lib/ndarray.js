

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Compute the inverse of a real upper or lower triangular matrix.
*
* @param {string} uplo - specifies the operation type
* @param {string} diag - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @returns {integer} status code (0 = success)
*/
function dtrtri( uplo, diag, N, A, strideA1, strideA2, offsetA ) { // eslint-disable-line max-len, max-params
	return base( uplo, diag, N, A, strideA1, strideA2, offsetA ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dtrtri;
