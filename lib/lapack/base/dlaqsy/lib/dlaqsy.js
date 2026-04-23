
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
* @param {Float64Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} s - s
* @param {integer} strideS - strideS
* @param {number} scond - scond
* @param {number} amax - amax
* @returns {*} result
*/
function dlaqsy( uplo, N, A, LDA, s, strideS, scond, amax ) { // eslint-disable-line max-len, max-params
	var sa1;
	var sa2;
	var os;

	sa1 = 1;
	sa2 = LDA;
	os = stride2offset( N, strideS );
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( LDA < max( 1, N ) ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be greater than or equal to max(1,N). Value: `%d`.', LDA ) );
	}
	return base( uplo, N, A, sa1, sa2, 0, s, strideS, os, scond, amax );
}


// EXPORTS //

module.exports = dlaqsy;
