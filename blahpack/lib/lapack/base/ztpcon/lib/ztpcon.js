
'use strict';

// MODULES //

var format = require( '@stdlib/string/format' );
var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var isDiagonalType = require( '@stdlib/blas/base/assert/is-diagonal-type' );
var base = require( './base.js' );


// MAIN //

/**
* Estimates the reciprocal condition number of a complex triangular matrix in packed storage.
*
* @param {string} norm - norm type: 'one-norm' or 'inf-norm'
* @param {string} uplo - 'upper' or 'lower'
* @param {string} diag - 'unit' or 'non-unit'
* @param {NonNegativeInteger} N - order of the matrix
* @param {Complex128Array} AP - packed triangular matrix
* @param {Float64Array} RCOND - output array of length 1
* @param {Complex128Array} WORK - workspace of length 2*N
* @param {Float64Array} RWORK - workspace of length N
* @returns {integer} info - 0 if successful
*/
function ztpcon( norm, uplo, diag, N, AP, RCOND, WORK, RWORK ) {
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( !isDiagonalType( diag ) ) {
		throw new TypeError( format( 'invalid argument. Third argument must be a valid diagonal type. Value: `%s`.', diag ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	return base( norm, uplo, diag, N, AP, 1, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 );
}


// EXPORTS //

module.exports = ztpcon;
