

'use strict';

// MODULES //

var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Copies a complex triangular matrix from full format (TR) to standard packed format (TP).
*
* @param {string} uplo - specifies whether `A` is upper or lower triangular (`'upper'` or `'lower'`)
* @param {NonNegativeInteger} N - order of the matrix `A`
* @param {Complex128Array} A - input matrix in full storage
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Complex128Array} AP - output array in packed storage
* @param {integer} strideAP - stride length for `AP`
* @param {NonNegativeInteger} offsetAP - starting index for `AP`
* @throws {TypeError} First argument must be a valid matrix triangle
* @returns {integer} status code (0 = success)
*/
function ztrttp( uplo, N, A, strideA1, strideA2, offsetA, AP, strideAP, offsetAP ) { // eslint-disable-line max-len, max-params
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	return base( uplo, N, A, strideA1, strideA2, offsetA, AP, strideAP, offsetAP ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = ztrttp;
