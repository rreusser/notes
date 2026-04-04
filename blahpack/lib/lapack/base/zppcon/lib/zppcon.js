
'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Estimates the reciprocal of the condition number of a complex Hermitian positive definite matrix in packed storage.
*
* @private
* @param {string} uplo - specifies whether the matrix is upper or lower triangular
* @param {NonNegativeInteger} N - order of the matrix
* @param {Complex128Array} AP - packed Cholesky factor
* @param {number} anorm - the 1-norm of the original matrix
* @param {Float64Array} rcond - output reciprocal condition number
* @param {Complex128Array} WORK - workspace array of length 2*N
* @param {Float64Array} RWORK - workspace array of length N
* @returns {integer} info - 0 if successful
*/
function zppcon( uplo, N, AP, anorm, rcond, WORK, RWORK ) {
	return base( uplo, N, AP, 1, 0, anorm, rcond, WORK, 1, 0, RWORK, 1, 0 );
}


// EXPORTS //

module.exports = zppcon;
