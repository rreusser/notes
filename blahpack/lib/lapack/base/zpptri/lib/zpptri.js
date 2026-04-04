
'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes the inverse of a complex Hermitian positive definite matrix in packed storage using the Cholesky factorization.
*
* @param {string} uplo - specifies whether the upper or lower triangle is stored
* @param {NonNegativeInteger} N - order of the matrix
* @param {Complex128Array} AP - packed Hermitian matrix
* @returns {integer} status code - `0` indicates success; `k > 0` indicates the `k`-th diagonal element of the Cholesky factor is zero
*/
function zpptri( uplo, N, AP ) {
	return base( uplo, N, AP, 1, 0 );
}


// EXPORTS //

module.exports = zpptri;
