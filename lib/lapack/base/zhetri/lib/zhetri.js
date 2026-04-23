
'use strict';

// MODULES //

var Complex128Array = require( '@stdlib/array/complex128' );
var format = require( '@stdlib/string/format' );
var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var max = require( '@stdlib/math/base/special/fast/max' );
var base = require( './base.js' );


// MAIN //

/**
* Computes the inverse of a complex Hermitian matrix using the factorization computed by zhetrf.
*
* @param {string} uplo - specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`)
* @param {NonNegativeInteger} N - order of the matrix
* @param {Complex128Array} A - Hermitian matrix (overwritten with inverse)
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Int32Array} IPIV - pivot indices from zhetrf
* @returns {integer} status code (0 = success)
*/
function zhetri( uplo, N, A, LDA, IPIV ) {
	var WORK;
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( LDA < max( 1, N ) ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be greater than or equal to max(1,N). Value: `%d`.', LDA ) );
	}
	WORK = new Complex128Array( N );
	return base( uplo, N, A, 1, LDA, 0, IPIV, 1, 0, WORK, 1, 0 );
}


// EXPORTS //

module.exports = zhetri;
