
'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes the inverse of a complex Hermitian positive definite matrix.
* stored in Rectangular Full Packed (RFP) format.
*
* @param {string} transr - specifies the storage format (`'no-transpose'` or `'conjugate-transpose'`)
* @param {string} uplo - specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`)
* @param {NonNegativeInteger} N - order of the matrix
* @param {Complex128Array} A - input/output array in RFP format
* @returns {integer} status code (0 = success)
*/
function zpftri( transr, uplo, N, A ) {
	return base( transr, uplo, N, A, 1, 0 );
}


// EXPORTS //

module.exports = zpftri;
