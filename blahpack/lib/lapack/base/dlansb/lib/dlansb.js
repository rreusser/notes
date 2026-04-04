'use strict';

// MODULES //

var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Returns the value of the one norm, Frobenius norm, infinity norm, or largest absolute value of a real symmetric band matrix using column-major order.
*
* @param {string} norm - norm type
* @param {string} uplo - specifies whether the upper or lower triangular part is stored
* @param {NonNegativeInteger} N - order of the matrix
* @param {NonNegativeInteger} K - number of super-diagonals or sub-diagonals
* @param {Float64Array} AB - band matrix
* @param {NonNegativeInteger} LDAB - leading dimension of `AB`
* @param {Float64Array} WORK - workspace array
* @throws {TypeError} second argument must be a valid matrix triangle
* @returns {number} norm value
*/
function dlansb( norm, uplo, N, K, AB, LDAB, WORK ) { // eslint-disable-line max-params
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	return base( norm, uplo, N, K, AB, 1, LDAB, 0, WORK, 1, 0 );
}


// EXPORTS //

module.exports = dlansb;
