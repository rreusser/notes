/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var isLayout = require( '@stdlib/blas/base/assert/is-layout' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Copies a triangular matrix from full format (TR) to standard packed format (TP).
*
* @param {string} order - storage layout (`'row-major'` or `'column-major'`)
* @param {string} uplo - specifies whether `A` is upper or lower triangular (`'upper'` or `'lower'`)
* @param {NonNegativeInteger} N - order of the matrix `A`
* @param {Float64Array} A - input matrix in full storage
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} AP - output array in packed storage
* @throws {TypeError} first argument must be a valid order
* @returns {integer} status code (0 = success)
*/
function dtrttp( order, uplo, N, A, LDA, AP ) {
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
	return base( uplo, N, A, sa1, sa2, 0, AP, 1, 0 );
}


// EXPORTS //

module.exports = dtrttp;
