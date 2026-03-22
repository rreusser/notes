'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Solves one of the complex matrix equations:
*   op(A)*X = alpha*B,  or  X*op(A) = alpha*B
*
* @param {string} side - 'L' or 'R'
* @param {string} uplo - 'U' or 'L'
* @param {string} transa - 'N', 'T', or 'C'
* @param {string} diag - 'U' or 'N'
* @param {NonNegativeInteger} M - rows of B
* @param {NonNegativeInteger} N - columns of B
* @param {Complex128} alpha - complex scalar
* @param {Complex128Array} A - complex triangular matrix
* @param {integer} strideA1 - first dimension stride of A
* @param {integer} strideA2 - second dimension stride of A
* @param {NonNegativeInteger} offsetA - offset for A (complex elements)
* @param {Complex128Array} B - complex input/output matrix
* @param {integer} strideB1 - first dimension stride of B
* @param {integer} strideB2 - second dimension stride of B
* @param {NonNegativeInteger} offsetB - offset for B (complex elements)
* @returns {Complex128Array} B
*/
function ztrsm( side, uplo, transa, diag, M, N, alpha, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB ) { // eslint-disable-line max-len, max-params
	return base( side, uplo, transa, diag, M, N, alpha, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = ztrsm;
