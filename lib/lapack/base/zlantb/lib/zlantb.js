
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var isDiagonalType = require( '@stdlib/blas/base/assert/is-diagonal-type' );
var max = require( '@stdlib/math/base/special/fast/max' );
var base = require( './base.js' );


// MAIN //

/**
* Returns the norm of a complex triangular band matrix.
*
* @param {string} norm - norm type
* @param {string} uplo - `'upper'` or `'lower'`
* @param {string} diag - `'unit'` or `'non-unit'`
* @param {NonNegativeInteger} N - order of the matrix
* @param {NonNegativeInteger} K - number of super/sub-diagonals
* @param {Complex128Array} AB - band matrix in band storage
* @param {PositiveInteger} LDAB - leading dimension of `AB`
* @param {Float64Array} WORK - workspace
* @param {integer} strideWORK - stride for WORK
* @returns {number} norm value
*/
function zlantb( norm, uplo, diag, N, K, AB, LDAB, WORK, strideWORK ) { // eslint-disable-line max-params
	var owork;
	var sa1;
	var sa2;

	sa1 = 1;
	sa2 = LDAB;
	owork = stride2offset( N, strideWORK );
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( norm !== 'max' && norm !== 'one-norm' && norm !== 'inf-norm' && norm !== 'frobenius' ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid norm. Value: `%s`.', norm ) );
	}
	if ( !isDiagonalType( diag ) ) {
		throw new TypeError( format( 'invalid argument. Third argument must be a valid diagonal type. Value: `%s`.', diag ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( K < 0 ) {
		throw new RangeError( format( 'invalid argument. Fifth argument must be a nonnegative integer. Value: `%d`.', K ) );
	}
	if ( LDAB < max( 1, N ) ) {
		throw new RangeError( format( 'invalid argument. Seventh argument must be greater than or equal to max(1,N). Value: `%d`.', LDAB ) );
	}
	return base( norm, uplo, diag, N, K, AB, sa1, sa2, 0, WORK, strideWORK, owork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zlantb;
