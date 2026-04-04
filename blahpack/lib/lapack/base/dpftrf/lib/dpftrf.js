
'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes the Cholesky factorization of a real symmetric positive definite matrix in Rectangular Full Packed format.
*
* @param {string} transr - specifies the storage format (`'no-transpose'` or `'transpose'`)
* @param {string} uplo - specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`)
* @param {NonNegativeInteger} N - order of the matrix
* @param {Float64Array} A - input/output array in RFP format
* @returns {integer} status code (0 = success)
*/
function dpftrf( transr, uplo, N, A ) {
	return base( transr, uplo, N, A, 1, 0 );
}


// EXPORTS //

module.exports = dpftrf;
