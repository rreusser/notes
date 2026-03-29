

'use strict';

// MODULES //

var isTransposeOperation = require( '@stdlib/blas/base/assert/is-transpose-operation' );
var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Copy a complex triangular matrix from Rectangular Full Packed (RFP) format to standard packed format (TP).
*
* @param {string} transr - specifies whether `ARF` is in normal or conjugate-transpose format (`'no-transpose'` or `'conjugate-transpose'`)
* @param {string} uplo - specifies whether the matrix is upper or lower triangular (`'upper'` or `'lower'`)
* @param {NonNegativeInteger} N - order of the matrix
* @param {Complex128Array} ARF - input array in RFP format
* @param {integer} strideARF - stride length for `ARF`
* @param {NonNegativeInteger} offsetARF - starting index for `ARF`
* @param {Complex128Array} AP - output array in standard packed format
* @param {integer} strideAP - stride length for `AP`
* @param {NonNegativeInteger} offsetAP - starting index for `AP`
* @throws {TypeError} First argument must be a valid transpose operation
* @throws {TypeError} Second argument must be a valid matrix triangle
* @returns {integer} status code (0 = success)
*/
function ztfttp( transr, uplo, N, ARF, strideARF, offsetARF, AP, strideAP, offsetAP ) { // eslint-disable-line max-len, max-params
	if ( !isTransposeOperation( transr ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid transpose operation. Value: `%s`.', transr ) );
	}
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	return base( transr, uplo, N, ARF, strideARF, offsetARF, AP, strideAP, offsetAP ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = ztfttp;
