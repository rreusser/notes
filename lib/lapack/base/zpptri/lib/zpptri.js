
'use strict';

// MODULES //

var format = require( '@stdlib/string/format' );
var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
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
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	return base( uplo, N, AP, 1, 0 );
}


// EXPORTS //

module.exports = zpptri;
