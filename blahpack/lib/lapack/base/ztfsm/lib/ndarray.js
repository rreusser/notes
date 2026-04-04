
'use strict';

// MODULES //

var isTransposeOperation = require( '@stdlib/blas/base/assert/is-transpose-operation' );
var isOperationSide = require( '@stdlib/blas/base/assert/is-operation-side' );
var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var isDiagonalType = require( '@stdlib/blas/base/assert/is-diagonal-type' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Solves a matrix equation with a complex triangular matrix in Rectangular Full Packed format.
*
* @param {string} transr - specifies the RFP storage format (`'no-transpose'` or `'conjugate-transpose'`)
* @param {string} side - specifies whether op(A) appears on the left or right (`'left'` or `'right'`)
* @param {string} uplo - specifies whether A is upper or lower triangular (`'upper'` or `'lower'`)
* @param {string} trans - specifies the operation applied to A (`'no-transpose'` or `'conjugate-transpose'`)
* @param {string} diag - specifies whether A is unit triangular (`'unit'` or `'non-unit'`)
* @param {NonNegativeInteger} M - number of rows of B
* @param {NonNegativeInteger} N - number of columns of B
* @param {Complex128} alpha - complex scalar multiplier
* @param {Complex128Array} A - RFP array
* @param {integer} strideA - stride length for `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Complex128Array} B - M-by-N complex matrix
* @param {integer} strideB1 - stride of the first dimension of `B`
* @param {integer} strideB2 - stride of the second dimension of `B`
* @param {NonNegativeInteger} offsetB - starting index for `B`
* @throws {TypeError} First argument must be a valid transpose operation
* @throws {TypeError} Second argument must be a valid operation side
* @throws {TypeError} Third argument must be a valid matrix triangle
* @throws {TypeError} Fourth argument must be a valid transpose operation
* @throws {TypeError} Fifth argument must be a valid diagonal type
* @returns {Complex128Array} `B`
*/
function ztfsm( transr, side, uplo, trans, diag, M, N, alpha, A, strideA, offsetA, B, strideB1, strideB2, offsetB ) { // eslint-disable-line max-len, max-params
	if ( !isTransposeOperation( transr ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid transpose operation. Value: `%s`.', transr ) );
	}
	if ( !isOperationSide( side ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid operation side. Value: `%s`.', side ) );
	}
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. Third argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( !isTransposeOperation( trans ) ) {
		throw new TypeError( format( 'invalid argument. Fourth argument must be a valid transpose operation. Value: `%s`.', trans ) );
	}
	if ( !isDiagonalType( diag ) ) {
		throw new TypeError( format( 'invalid argument. Fifth argument must be a valid diagonal type. Value: `%s`.', diag ) );
	}
	return base( transr, side, uplo, trans, diag, M, N, alpha, A, strideA, offsetA, B, strideB1, strideB2, offsetB ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = ztfsm;
