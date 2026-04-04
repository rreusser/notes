'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Performs a symmetric rank-k operation for a matrix in Rectangular Full Packed format.
*
* @param {string} transr - specifies the storage format (`'no-transpose'` or `'transpose'`)
* @param {string} uplo - specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`)
* @param {string} trans - specifies the operation (`'no-transpose'` or `'transpose'`)
* @param {NonNegativeInteger} N - order of the symmetric matrix C
* @param {NonNegativeInteger} K - number of columns of A (if trans='no-transpose') or rows (if trans='transpose')
* @param {number} alpha - scalar multiplier for A*A^T or A^T*A
* @param {Float64Array} A - input matrix
* @param {NonNegativeInteger} LDA - leading dimension of `A`
* @param {number} beta - scalar multiplier for C
* @param {Float64Array} C - RFP array of length N*(N+1)/2
* @returns {Float64Array} `C`
*/
function dsfrk( transr, uplo, trans, N, K, alpha, A, LDA, beta, C ) {
	return base( transr, uplo, trans, N, K, alpha, A, 1, LDA, 0, beta, C, 1, 0 ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dsfrk;
