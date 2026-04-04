'use strict';

// MODULES //

var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var isDiagonalType = require( '@stdlib/blas/base/assert/is-diagonal-type' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Returns the norm of a real triangular band matrix.
*
* @param {string} norm - specifies the operation type
* @param {string} uplo - specifies the operation type
* @param {string} diag - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {NonNegativeInteger} K - number of superdiagonals
* @param {Float64Array} AB - input matrix
* @param {integer} strideAB1 - stride of the first dimension of `AB`
* @param {integer} strideAB2 - stride of the second dimension of `AB`
* @param {NonNegativeInteger} offsetAB - starting index for `AB`
* @param {Float64Array} WORK - output array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @throws {TypeError} Second argument must be a valid matrix triangle
* @throws {TypeError} Third argument must be a valid diagonal type
* @returns {number} result
*/
function dlantb( norm, uplo, diag, N, K, AB, strideAB1, strideAB2, offsetAB, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( !isDiagonalType( diag ) ) {
		throw new TypeError( format( 'invalid argument. Third argument must be a valid diagonal type. Value: `%s`.', diag ) );
	}
	return base( norm, uplo, diag, N, K, AB, strideAB1, strideAB2, offsetAB, WORK, strideWORK, offsetWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlantb;
