
'use strict';

// MODULES //

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
	return base( norm, uplo, diag, N, AP, 1, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 );
}


// EXPORTS //

module.exports = ztpcon;
