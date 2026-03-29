
'use strict';

// MODULES //

var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Computes row and column scalings intended to equilibrate a symmetric positive definite matrix in packed storage and reduce its condition number.
*
* @param {string} uplo - specifies whether the upper or lower triangle is stored (`upper` or `lower`)
* @param {NonNegativeInteger} N - order of the matrix A
* @param {Float64Array} AP - input symmetric positive definite matrix in packed storage
* @param {integer} strideAP - stride length for `AP`
* @param {NonNegativeInteger} offsetAP - starting index for `AP`
* @param {Float64Array} s - output scale factors, length N
* @param {integer} strideS - stride length for `s`
* @param {NonNegativeInteger} offsetS - starting index for `s`
* @throws {TypeError} First argument must be a valid matrix triangle
* @returns {Object} result with `info`, `scond`, and `amax` properties
*/
function dppequ( uplo, N, AP, strideAP, offsetAP, s, strideS, offsetS ) { // eslint-disable-line max-len, max-params
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	return base( uplo, N, AP, strideAP, offsetAP, s, strideS, offsetS ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dppequ;
