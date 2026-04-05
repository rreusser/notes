
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var isMatrixTranspose = require( '@stdlib/blas/base/assert/is-transpose-operation' );
var isDiagonalType = require( '@stdlib/blas/base/assert/is-diagonal-type' );
var base = require( './base.js' );


// MAIN //

/**
* Provides error bounds and backward error estimates for the solution to a system of linear equations with a packed triangular coefficient matrix.
*
* @param {string} uplo - specifies whether the matrix is upper or lower triangular
* @param {string} trans - specifies the form of the system of equations
* @param {string} diag - specifies whether the matrix is unit triangular
* @param {NonNegativeInteger} N - order of the matrix A
* @param {NonNegativeInteger} nrhs - number of right-hand sides
* @param {Float64Array} AP - packed triangular matrix A
* @param {Float64Array} B - right-hand side matrix B
* @param {PositiveInteger} LDB - leading dimension of `B`
* @param {Float64Array} X - solution matrix X
* @param {PositiveInteger} LDX - leading dimension of `X`
* @param {Float64Array} FERR - forward error bounds
* @param {integer} strideFERR - stride for `FERR`
* @param {Float64Array} BERR - backward errors
* @param {integer} strideBERR - stride for `BERR`
* @param {Float64Array} WORK - workspace array
* @param {integer} strideWORK - stride for `WORK`
* @param {Int32Array} IWORK - integer workspace array
* @param {integer} strideIWORK - stride for `IWORK`
* @returns {integer} status code (0 = success)
*/
function dtprfs( uplo, trans, diag, N, nrhs, AP, B, LDB, X, LDX, FERR, strideFERR, BERR, strideBERR, WORK, strideWORK, IWORK, strideIWORK ) { // eslint-disable-line max-len, max-params
	var oiwork;
	var oberr;
	var oferr;
	var owork;

	oferr = stride2offset( nrhs, strideFERR );
	oberr = stride2offset( nrhs, strideBERR );
	owork = stride2offset( 3 * N, strideWORK );
	oiwork = stride2offset( N, strideIWORK );
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
		throw new RangeError( format( 'invalid argument. Fourth argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( nrhs < 0 ) {
		throw new RangeError( format( 'invalid argument. Fifth argument must be a nonnegative integer. Value: `%d`.', nrhs ) );
	}
	return base( uplo, trans, diag, N, nrhs, AP, 1, 0, B, 1, LDB, 0, X, 1, LDX, 0, FERR, strideFERR, oferr, BERR, strideBERR, oberr, WORK, strideWORK, owork, IWORK, strideIWORK, oiwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dtprfs;
