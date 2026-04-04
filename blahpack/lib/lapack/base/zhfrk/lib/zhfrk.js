
'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Performs a Hermitian rank-k operation for a matrix in Rectangular Full Packed format.
*
* C := alpha\*A\*A^H + beta\*C, or C := alpha\*A^H\*A + beta\*C,
* where alpha and beta are real scalars, C is an N-by-N Hermitian matrix
* stored in RFP format, and A is an N-by-K or K-by-N complex matrix.
*
* @param {string} transr - specifies the storage format (`'no-transpose'` or `'conjugate-transpose'`)
* @param {string} uplo - specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`)
* @param {string} trans - specifies the operation (`'no-transpose'` or `'conjugate-transpose'`)
* @param {NonNegativeInteger} N - order of Hermitian matrix C
* @param {NonNegativeInteger} K - number of columns of A (if trans='no-transpose') or rows (if trans='conjugate-transpose')
* @param {number} alpha - real scalar multiplier for A\*A^H or A^H\*A
* @param {Complex128Array} A - complex input matrix
* @param {NonNegativeInteger} LDA - leading dimension of `A`
* @param {number} beta - real scalar multiplier for C
* @param {Complex128Array} C - RFP array of length N*(N+1)/2
* @returns {Complex128Array} `C`
*/
function zhfrk( transr, uplo, trans, N, K, alpha, A, LDA, beta, C ) {
	return base( transr, uplo, trans, N, K, alpha, A, 1, LDA, 0, beta, C, 1, 0 ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zhfrk;
