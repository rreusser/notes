

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var max = require( '@stdlib/math/base/special/fast/max' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} uplo - uplo
* @param {NonNegativeInteger} N - N
* @param {Complex128Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Int32Array} IPIV - IPIV
* @param {integer} strideIPIV - strideIPIV
* @returns {*} result
*/
function zhetf2( uplo, N, A, LDA, IPIV, strideIPIV ) {
	var oipiv;
	var sa1;
	var sa2;

	sa1 = 1;
	sa2 = LDA;
	oipiv = stride2offset( N, strideIPIV );
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( LDA < max( 1, N ) ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be greater than or equal to max(1,N). Value: `%d`.', LDA ) );
	}
	return base( uplo, N, A, sa1, sa2, 0, IPIV, strideIPIV, oipiv );
}


// EXPORTS //

module.exports = zhetf2;
