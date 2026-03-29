/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var isLayout = require( '@stdlib/blas/base/assert/is-layout' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Copies a complex triangular matrix from standard full format (TR) to Rectangular Full Packed format (TF).
*
* @param {string} order - storage layout (`'row-major'` or `'column-major'`)
* @param {string} transr - specifies whether the RFP format is in normal or conjugate-transpose mode (`'no-transpose'` or `'conjugate-transpose'`)
* @param {string} uplo - specifies whether the matrix is upper or lower triangular (`'upper'` or `'lower'`)
* @param {NonNegativeInteger} N - order of the matrix
* @param {Complex128Array} A - input triangular matrix in standard full format
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Complex128Array} ARF - output array in RFP format
* @throws {TypeError} first argument must be a valid order
* @returns {integer} status code (0 = success)
*/
function ztrttf( order, transr, uplo, N, A, LDA, ARF ) {
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
	return base( transr, uplo, N, A, sa1, sa2, 0, LDA, ARF, 1, 0 );
}


// EXPORTS //

module.exports = ztrttf;
