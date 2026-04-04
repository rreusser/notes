

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Solves a complex triangular system with scaling to prevent overflow, where the matrix is in packed storage.
*
* @private
* @param {string} uplo - specifies whether the matrix is upper or lower triangular
* @param {string} trans - specifies the operation applied to A
* @param {string} diag - specifies whether the matrix is unit or non-unit triangular
* @param {string} normin - specifies whether CNORM is pre-computed
* @param {NonNegativeInteger} N - order of the matrix
* @param {Complex128Array} AP - packed triangular matrix
* @param {Complex128Array} x - right-hand side vector
* @param {Float64Array} scale - output scale factor
* @param {Float64Array} CNORM - column norm array
* @returns {integer} info - 0 if successful
*/
function zlatps( uplo, trans, diag, normin, N, AP, x, scale, CNORM ) { // eslint-disable-line max-params
	return base( uplo, trans, diag, normin, N, AP, 1, 0, x, 1, 0, scale, CNORM, 1, 0 );
}


// EXPORTS //

module.exports = zlatps;
