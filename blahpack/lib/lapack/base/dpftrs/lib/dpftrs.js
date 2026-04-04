
'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Solves a system of linear equations `A * X = B` with a symmetric positive definite matrix in Rectangular Full Packed format.
*
* @param {string} transr - specifies the storage format (`'no-transpose'` or `'transpose'`)
* @param {string} uplo - specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`)
* @param {NonNegativeInteger} N - order of the matrix
* @param {NonNegativeInteger} nrhs - number of right-hand side columns
* @param {Float64Array} A - RFP array (Cholesky factor from dpftrf)
* @param {Float64Array} B - right-hand side matrix (column-major, LDB = N)
* @param {PositiveInteger} LDB - leading dimension of B
* @returns {integer} status code (0 = success)
*/
function dpftrs( transr, uplo, N, nrhs, A, B, LDB ) {
	return base( transr, uplo, N, nrhs, A, 1, 0, B, 1, LDB, 0 );
}


// EXPORTS //

module.exports = dpftrs;
