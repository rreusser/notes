

'use strict';

// MODULES //

var isDiagonalType = require( '@stdlib/blas/base/assert/is-diagonal-type' );
var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var isTransposeOperation = require( '@stdlib/blas/base/assert/is-transpose-operation' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
 * Performs one of the matrix-vector operations `x := A*x` or `x := A^T*x`.
 *
 * `x` is an N element vector and A is an N by N unit or non-unit, upper or
 * lower triangular matrix, supplied in packed form.
 *
 *
 * @param {string} uplo - specifies whether the matrix is upper or lower triangular
 * @param {string} trans - specifies the operation to perform
 * @param {string} diag - specifies whether the matrix is unit triangular
 * @param {NonNegativeInteger} N - order of the matrix A
 * @param {Float64Array} AP - packed triangular matrix
 * @param {integer} strideAP - stride length for `AP`
 * @param {NonNegativeInteger} offsetAP - starting index for `AP`
 * @param {Float64Array} x - input/output vector
 * @param {integer} strideX - stride length for `x`
 * @param {NonNegativeInteger} offsetX - starting index for `x`
 * @throws {TypeError} First argument must be a valid matrix triangle
 * @throws {TypeError} Second argument must be a valid transpose operation
 * @throws {TypeError} Third argument must be a valid diagonal type
 * @returns {Float64Array} `x`
 */
function dtpmv( uplo, trans, diag, N, AP, strideAP, offsetAP, x, strideX, offsetX ) { // eslint-disable-line max-len, max-params
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( !isTransposeOperation( trans ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid transpose operation. Value: `%s`.', trans ) );
	}
	if ( !isDiagonalType( diag ) ) {
		throw new TypeError( format( 'invalid argument. Third argument must be a valid diagonal type. Value: `%s`.', diag ) );
	}
	return base( uplo, trans, diag, N, AP, strideAP, offsetAP, x, strideX, offsetX ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dtpmv;
