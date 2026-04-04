
'use strict';

// MODULES //

var isTransposeOperation = require( '@stdlib/blas/base/assert/is-transpose-operation' );
var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Computes the Cholesky factorization of a real symmetric positive definite matrix in Rectangular Full Packed format.
*
* @param {string} transr - specifies the storage format (`'no-transpose'` or `'transpose'`)
* @param {string} uplo - specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`)
* @param {NonNegativeInteger} N - order of the matrix
* @param {Float64Array} A - input/output array in RFP format
* @param {integer} strideA - stride length for `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @throws {TypeError} First argument must be a valid transpose operation
* @throws {TypeError} Second argument must be a valid matrix triangle
* @returns {integer} status code (0 = success)
*/
function dpftrf( transr, uplo, N, A, strideA, offsetA ) {
	if ( !isTransposeOperation( transr ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid transpose operation. Value: `%s`.', transr ) );
	}
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	return base( transr, uplo, N, A, strideA, offsetA );
}


// EXPORTS //

module.exports = dpftrf;
