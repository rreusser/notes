
'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Solves a matrix equation with a complex triangular matrix in Rectangular Full Packed format.
*
* @param {string} transr - specifies the RFP storage format (`'no-transpose'` or `'conjugate-transpose'`)
* @param {string} side - specifies whether op(A) appears on the left or right (`'left'` or `'right'`)
* @param {string} uplo - specifies whether A is upper or lower triangular (`'upper'` or `'lower'`)
* @param {string} trans - specifies the operation applied to A (`'no-transpose'` or `'conjugate-transpose'`)
* @param {string} diag - specifies whether A is unit triangular (`'unit'` or `'non-unit'`)
* @param {NonNegativeInteger} M - number of rows of B
* @param {NonNegativeInteger} N - number of columns of B
* @param {Complex128} alpha - complex scalar multiplier
* @param {Complex128Array} A - RFP array
* @param {Complex128Array} B - M-by-N complex matrix (column-major, tightly packed)
* @returns {Complex128Array} `B`
*/
function ztfsm( transr, side, uplo, trans, diag, M, N, alpha, A, B ) {
	return base( transr, side, uplo, trans, diag, M, N, alpha, A, 1, 0, B, 1, M, 0 ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = ztfsm;
