

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes row and column scalings intended to equilibrate a complex Hermitian positive definite matrix in packed storage and reduce its condition number.
*
* @param {string} uplo - specifies whether the upper or lower triangle is stored
* @param {NonNegativeInteger} N - order of the matrix A
* @param {Complex128Array} AP - input Hermitian positive definite matrix in packed storage
* @param {Float64Array} s - output scale factors, length N
* @returns {Object} result with `info`, `scond`, and `amax` properties
*/
function zppequ( uplo, N, AP, s ) {
	return base( uplo, N, AP, 1, 0, s, 1, 0 );
}


// EXPORTS //

module.exports = zppequ;
