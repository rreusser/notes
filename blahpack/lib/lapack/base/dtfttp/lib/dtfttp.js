
'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Copies a triangular matrix from Rectangular Full Packed (RFP) format to standard packed format (TP).
*
* @param {string} transr - specifies whether `ARF` is in normal or transposed format (`'no-transpose'` or `'transpose'`)
* @param {string} uplo - specifies whether the matrix is upper or lower triangular (`'upper'` or `'lower'`)
* @param {NonNegativeInteger} N - order of the matrix
* @param {Float64Array} ARF - input array in RFP format
* @param {Float64Array} AP - output array in standard packed format
* @returns {integer} status code (0 = success)
*/
function dtfttp( transr, uplo, N, ARF, AP ) {
	return base( transr, uplo, N, ARF, 1, 0, AP, 1, 0 );
}


// EXPORTS //

module.exports = dtfttp;
