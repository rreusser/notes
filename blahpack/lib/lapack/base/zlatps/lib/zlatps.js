

'use strict';

// MODULES //

var format = require( '@stdlib/string/format' );
var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var isMatrixTranspose = require( '@stdlib/blas/base/assert/is-transpose-operation' );
var isDiagonalType = require( '@stdlib/blas/base/assert/is-diagonal-type' );
var base = require( './base.js' );


// MAIN //

/**
* Solves a complex triangular system with scaling to prevent overflow, where the matrix is in packed storage.
*
* @private
* @param {string} uplo - specifies whether the matrix is upper or lower triangular
* @param {string} trans - specifies the operation applied to A
* @param {string} diag - specifies whether the matrix is unit or non-unit triangular
* @param {string} normin - specifies whether CNORM is pre-computed
* @param {NonNegativeInteger} N - order of the matrix
* @param {Complex128Array} AP - packed triangular matrix
* @param {Complex128Array} x - right-hand side vector
* @param {Float64Array} scale - output scale factor
* @param {Float64Array} CNORM - column norm array
* @returns {integer} info - 0 if successful
*/
function zlatps( uplo, trans, diag, normin, N, AP, x, scale, CNORM ) { // eslint-disable-line max-params
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( !isMatrixTranspose( trans ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid transpose operation. Value: `%s`.', trans ) );
	}
	if ( !isDiagonalType( diag ) ) {
		throw new TypeError( format( 'invalid argument. Third argument must be a valid diagonal type. Value: `%s`.', diag ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Fifth argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	return base( uplo, trans, diag, normin, N, AP, 1, 0, x, 1, 0, scale, CNORM, 1, 0 );
}


// EXPORTS //

module.exports = zlatps;
