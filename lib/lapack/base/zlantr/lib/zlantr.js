

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
* @license Apache-2.0
*
* @param {string} norm - norm
* @param {string} uplo - uplo
* @param {string} diag - diag
* @param {NonNegativeInteger} M - M
* @param {NonNegativeInteger} N - N
* @param {Complex128Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @returns {*} result
*/
function zlantr( norm, uplo, diag, M, N, A, LDA, WORK, strideWORK ) { // eslint-disable-line max-len, max-params
	var owork;
	var sa1;
	var sa2;

	sa1 = 1;
	sa2 = LDA;
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
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Fifth argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( LDA < max( 1, M ) ) {
		throw new RangeError( format( 'invalid argument. Seventh argument must be greater than or equal to max(1,M). Value: `%d`.', LDA ) );
	}
	return base( norm, uplo, diag, M, N, A, sa1, sa2, 0, WORK, strideWORK, owork );
}


// EXPORTS //

module.exports = zlantr;
