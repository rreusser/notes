
'use strict';

// MODULES //

var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Compute the Cholesky factorization of a real symmetric positive definite matrix stored in packed format.
*
* @param {string} uplo - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} AP - input array
* @param {integer} stride - stride length for `AP`
* @param {NonNegativeInteger} offset - starting index for `AP`
* @throws {TypeError} First argument must be a valid matrix triangle
* @returns {integer} status code (0 = success)
*/
function dpptrf( uplo, N, AP, stride, offset ) {
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	return base( uplo, N, AP, stride, offset );
}


// EXPORTS //

module.exports = dpptrf;
