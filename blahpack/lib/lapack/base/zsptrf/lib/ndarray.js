
'use strict';

// MODULES //

var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Computes the Bunch-Kaufman factorization of a complex symmetric matrix in packed storage.
*
* @param {string} uplo - specifies the operation type
* @param {NonNegativeInteger} N - order of the matrix A
* @param {Complex128Array} AP - packed symmetric matrix, length N*(N+1)/2
* @param {integer} strideAP - stride length for `AP` (in complex elements)
* @param {NonNegativeInteger} offsetAP - starting index for `AP` (in complex elements)
* @param {Int32Array} IPIV - pivot index output array, length N
* @param {integer} strideIPIV - stride for IPIV
* @param {NonNegativeInteger} offsetIPIV - index offset for IPIV
* @throws {TypeError} First argument must be a valid matrix triangle
* @returns {integer} status code (0 = success)
*/
function zsptrf( uplo, N, AP, strideAP, offsetAP, IPIV, strideIPIV, offsetIPIV ) { // eslint-disable-line max-len, max-params
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	return base( uplo, N, AP, strideAP, offsetAP, IPIV, strideIPIV, offsetIPIV ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zsptrf;
