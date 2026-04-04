'use strict';

// MODULES //

var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var isDiagonalType = require( '@stdlib/blas/base/assert/is-diagonal-type' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Estimates the reciprocal condition number of a real triangular band matrix using column-major order.
*
* @param {string} norm - norm type: 'one-norm' or 'inf-norm'
* @param {string} uplo - 'upper' or 'lower'
* @param {string} diag - 'unit' or 'non-unit'
* @param {NonNegativeInteger} N - order of the matrix
* @param {NonNegativeInteger} kd - number of superdiagonals (upper) or subdiagonals (lower)
* @param {Float64Array} AB - triangular band matrix in band storage
* @param {NonNegativeInteger} LDAB - leading dimension of `AB`
* @param {Float64Array} rcond - output array of length 1
* @param {Float64Array} WORK - workspace array of length at least 3*N
* @param {Int32Array} IWORK - integer workspace array of length at least N
* @throws {TypeError} second argument must be a valid matrix triangle
* @throws {TypeError} third argument must be a valid diagonal type
* @returns {integer} info - 0 if successful
*/
function dtbcon( norm, uplo, diag, N, kd, AB, LDAB, rcond, WORK, IWORK ) { // eslint-disable-line max-params
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( !isDiagonalType( diag ) ) {
		throw new TypeError( format( 'invalid argument. Third argument must be a valid diagonal type. Value: `%s`.', diag ) );
	}
	return base( norm, uplo, diag, N, kd, AB, 1, LDAB, 0, rcond, WORK, 1, 0, IWORK, 1, 0 ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dtbcon;
