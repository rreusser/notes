

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Compute the inverse of a complex upper or lower triangular matrix (unblocked algorithm).
*
* @param {string} uplo - specifies whether the matrix is upper or lower triangular
* @param {string} diag - specifies whether the matrix is unit or non-unit triangular
* @param {NonNegativeInteger} N - order of the matrix
* @param {Complex128Array} A - input/output triangular matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @returns {integer} status code (0 = success)
*/
function ztrti2( uplo, diag, N, A, strideA1, strideA2, offsetA ) { // eslint-disable-line max-len, max-params
	return base( uplo, diag, N, A, strideA1, strideA2, offsetA ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = ztrti2;
