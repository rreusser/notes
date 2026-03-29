/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var isLayout = require( '@stdlib/blas/base/assert/is-layout' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Copies a triangular matrix from Rectangular Full Packed (RFP) format to standard full format.
*
* @param {string} order - storage layout (`'row-major'` or `'column-major'`)
* @param {string} transr - specifies whether `ARF` is in normal or transposed format (`'no-transpose'` or `'transpose'`)
* @param {string} uplo - specifies whether the matrix is upper or lower triangular (`'upper'` or `'lower'`)
* @param {NonNegativeInteger} N - order of the matrix
* @param {Float64Array} ARF - input array in RFP format
* @param {Float64Array} A - output matrix in full format
* @param {PositiveInteger} LDA - leading dimension of `A`
* @throws {TypeError} first argument must be a valid order
* @returns {integer} status code (0 = success)
*/
function dtfttr( order, transr, uplo, N, ARF, A, LDA ) {
	var sa1;
	var sa2;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( order === 'column-major' ) {
		sa1 = 1;
		sa2 = LDA;
	} else {
		sa1 = LDA;
		sa2 = 1;
	}
	return base( transr, uplo, N, ARF, 1, 0, A, sa1, sa2, 0, LDA );
}


// EXPORTS //

module.exports = dtfttr;
