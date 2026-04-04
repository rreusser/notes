'use strict';

// MODULES //

var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Returns the norm of a complex Hermitian band matrix.
*
* @param {string} norm - norm type: `'max'`, `'one-norm'`, `'inf-norm'`, or `'frobenius'`
* @param {string} uplo - specifies whether the upper or lower triangular part is stored: `'upper'` or `'lower'`
* @param {NonNegativeInteger} N - order of the matrix
* @param {NonNegativeInteger} K - number of super-diagonals (upper) or sub-diagonals (lower)
* @param {Complex128Array} AB - band matrix in band storage
* @param {integer} strideAB1 - stride of the first dimension of `AB` (in complex elements)
* @param {integer} strideAB2 - stride of the second dimension of `AB` (in complex elements)
* @param {NonNegativeInteger} offsetAB - starting index for `AB` (in complex elements)
* @param {Float64Array} WORK - workspace array (length >= N for `'one-norm'` or `'inf-norm'`)
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @throws {TypeError} Second argument must be a valid matrix triangle
* @returns {number} result
*/
function zlanhb( norm, uplo, N, K, AB, strideAB1, strideAB2, offsetAB, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	return base( norm, uplo, N, K, AB, strideAB1, strideAB2, offsetAB, WORK, strideWORK, offsetWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zlanhb;
