
'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Copies a triangular matrix from standard packed format (TP) to Rectangular Full Packed format (RFP).
*
* @param {string} transr - specifies whether `ARF` is in normal or transposed format (`'no-transpose'` or `'transpose'`)
* @param {string} uplo - specifies whether the matrix is upper or lower triangular (`'upper'` or `'lower'`)
* @param {NonNegativeInteger} N - order of the matrix
* @param {Float64Array} AP - input array in standard packed format
* @param {Float64Array} ARF - output array in RFP format
* @returns {integer} status code (0 = success)
*/
function dtpttf( transr, uplo, N, AP, ARF ) {
	return base( transr, uplo, N, AP, 1, 0, ARF, 1, 0 );
}


// EXPORTS //

module.exports = dtpttf;
