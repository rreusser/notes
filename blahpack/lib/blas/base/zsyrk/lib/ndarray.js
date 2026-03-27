

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Perform one of the symmetric rank-k operations C := alpha*A*A**T + beta*C or C := alpha*A**T*A + beta*C.
*
* @param {string} uplo - specifies the operation type
* @param {string} trans - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {NonNegativeInteger} K - number of superdiagonals
* @param {Complex128} alpha - scalar constant
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Complex128} beta - scalar constant
* @param {Float64Array} C - output matrix
* @param {integer} strideC1 - stride of the first dimension of `C`
* @param {integer} strideC2 - stride of the second dimension of `C`
* @param {NonNegativeInteger} offsetC - starting index for `C`
*/
function zsyrk( uplo, trans, N, K, alpha, A, strideA1, strideA2, offsetA, beta, C, strideC1, strideC2, offsetC ) { // eslint-disable-line max-len, max-params
	return base( uplo, trans, N, K, alpha, A, strideA1, strideA2, offsetA, beta, C, strideC1, strideC2, offsetC ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zsyrk;
