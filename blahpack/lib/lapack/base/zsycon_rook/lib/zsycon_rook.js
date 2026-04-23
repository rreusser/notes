/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var isLayout = require( '@stdlib/blas/base/assert/is-layout' );
var format = require( '@stdlib/string/format' );
var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var max = require( '@stdlib/math/base/special/fast/max' );
var base = require( './base.js' );


// MAIN //

/**
* Estimate reciprocal condition number of a complex symmetric matrix using rook-pivoted factorization.
*
* @param {string} order - storage layout (`'row-major'` or `'column-major'`)
* @param {string} uplo - specifies the storage triangle
* @param {NonNegativeInteger} N - order of the matrix `A`
* @param {Complex128Array} A - factored matrix from `zsytrf_rook`
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Int32Array} IPIV - input array
* @param {integer} strideIPIV - stride length for `IPIV`
* @param {NonNegativeInteger} offsetIPIV - starting index for `IPIV`
* @param {number} anorm - the 1-norm of the original matrix `A`
* @param {Float64Array} rcond - output: `rcond[0]` receives the reciprocal condition number
* @param {Complex128Array} WORK - workspace array of length at least `2*N`
* @param {integer} strideWORK - stride length for `WORK`
* @throws {TypeError} first argument must be a valid order
* @throws {TypeError} Second argument must be a valid matrix triangle
* @throws {RangeError} Third argument must be a nonnegative integer
* @throws {RangeError} Fifth argument must be greater than or equal to max(1,N)
* @returns {integer} status code (0 = success)
*/
function zsyconRook( order, uplo, N, A, LDA, IPIV, strideIPIV, offsetIPIV, anorm, rcond, WORK, strideWORK ) {
	var sa1;
	var sa2;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Third argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( order === 'row-major' && LDA < max( 1, N ) ) {
		throw new RangeError( format( 'invalid argument. Fifth argument must be greater than or equal to max(1,N). Value: `%d`.', LDA ) );
	}
	if ( order === 'column-major' ) {
		sa1 = 1;
		sa2 = LDA;
	} else {
		sa1 = LDA;
		sa2 = 1;
	}
	return base( uplo, N, A, sa1, sa2, 0, IPIV, strideIPIV, offsetIPIV, anorm, rcond, WORK, strideWORK, 0 );
}


// EXPORTS //

module.exports = zsyconRook;
