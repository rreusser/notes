

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes the norm of a complex triangular matrix
*
* @param {string} norm - specifies the operation type
* @param {string} uplo - specifies the operation type
* @param {string} diag - specifies the operation type
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} WORK - output array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @returns {number} result
*/
function zlantr( norm, uplo, diag, M, N, A, strideA1, strideA2, offsetA, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	return base( norm, uplo, diag, M, N, A, strideA1, strideA2, offsetA, WORK, strideWORK, offsetWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zlantr;
