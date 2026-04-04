
'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes the inverse of a complex triangular matrix in Rectangular Full Packed format.
*
* @param {string} transr - specifies the storage format (`'no-transpose'` or `'conjugate-transpose'`)
* @param {string} uplo - specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`)
* @param {string} diag - specifies whether the matrix is unit triangular (`'unit'` or `'non-unit'`)
* @param {NonNegativeInteger} N - order of the matrix
* @param {Complex128Array} A - input/output array in RFP format
* @returns {integer} status code (0 = success)
*/
function ztftri( transr, uplo, diag, N, A ) {
	return base( transr, uplo, diag, N, A, 1, 0 );
}


// EXPORTS //

module.exports = ztftri;
