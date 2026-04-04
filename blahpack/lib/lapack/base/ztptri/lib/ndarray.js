
'use strict';

// MODULES //

var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var isDiagonalType = require( '@stdlib/blas/base/assert/is-diagonal-type' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Computes the inverse of a complex triangular matrix in packed storage.
*
* @param {string} uplo - specifies the operation type
* @param {string} diag - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} AP - input array
* @param {integer} stride - stride length for `AP`
* @param {NonNegativeInteger} offset - starting index for `AP`
* @throws {TypeError} First argument must be a valid matrix triangle
* @throws {TypeError} Second argument must be a valid diagonal type
* @returns {integer} status code (0 = success)
*/
function ztptri( uplo, diag, N, AP, stride, offset ) {
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( !isDiagonalType( diag ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid diagonal type. Value: `%s`.', diag ) );
	}
	return base( uplo, diag, N, AP, stride, offset );
}


// EXPORTS //

module.exports = ztptri;
