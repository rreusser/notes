
'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Solves a matrix equation with a triangular matrix in Rectangular Full Packed format.
*
* @param {string} transr - specifies the storage format (`'no-transpose'` or `'transpose'`)
* @param {string} side - specifies whether op(A) appears on the left or right (`'left'` or `'right'`)
* @param {string} uplo - specifies whether A is upper or lower triangular (`'upper'` or `'lower'`)
* @param {string} trans - specifies the transpose operation on A (`'no-transpose'` or `'transpose'`)
* @param {string} diag - specifies whether A has unit diagonal (`'unit'` or `'non-unit'`)
* @param {NonNegativeInteger} M - number of rows of B
* @param {NonNegativeInteger} N - number of columns of B
* @param {number} alpha - scalar multiplier
* @param {Float64Array} A - RFP array
* @param {Float64Array} B - M-by-N input/output matrix (column-major)
* @param {NonNegativeInteger} LDB - leading dimension of B
* @returns {void}
*/
function dtfsm( transr, side, uplo, trans, diag, M, N, alpha, A, B, LDB ) { // eslint-disable-line max-params
	base( transr, side, uplo, trans, diag, M, N, alpha, A, 1, 0, B, 1, LDB, 0 );
}


// EXPORTS //

module.exports = dtfsm;
