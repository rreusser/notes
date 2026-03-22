

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Performs a Hermitian rank-k update (complex double-precision).
*
* @param {string} uplo - specifies upper or lower triangle
* @param {string} trans - 'N' or 'C'
* @param {NonNegativeInteger} N - order of matrix C
* @param {NonNegativeInteger} K - dimension parameter
* @param {number} alpha - real scalar
* @param {Complex128Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {number} beta - real scalar
* @param {Complex128Array} C - output matrix
* @param {integer} strideC1 - stride of the first dimension of `C`
* @param {integer} strideC2 - stride of the second dimension of `C`
* @param {NonNegativeInteger} offsetC - starting index for `C`
*/
function zherk( uplo, trans, N, K, alpha, A, strideA1, strideA2, offsetA, beta, C, strideC1, strideC2, offsetC ) { // eslint-disable-line max-len, max-params
	return base( uplo, trans, N, K, alpha, A, strideA1, strideA2, offsetA, beta, C, strideC1, strideC2, offsetC ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zherk;
