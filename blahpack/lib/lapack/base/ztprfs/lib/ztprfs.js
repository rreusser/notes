
'use strict';

// MODULES //

var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var isTransposeOperation = require( '@stdlib/blas/base/assert/is-transpose-operation' );
var isDiagonalType = require( '@stdlib/blas/base/assert/is-diagonal-type' );
var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Provides error bounds and backward error estimates for the solution to a system of linear equations with a packed complex triangular coefficient matrix.
*
* @param {string} uplo - specifies whether the matrix is upper or lower triangular
* @param {string} trans - specifies the form of the system of equations
* @param {string} diag - specifies whether the matrix is unit triangular
* @param {NonNegativeInteger} N - order of the matrix A
* @param {NonNegativeInteger} nrhs - number of right-hand sides
* @param {Complex128Array} AP - packed triangular matrix A
* @param {Complex128Array} B - right-hand side matrix B
* @param {PositiveInteger} LDB - leading dimension of `B`
* @param {Complex128Array} X - solution matrix X
* @param {PositiveInteger} LDX - leading dimension of `X`
* @param {Float64Array} FERR - forward error bounds
* @param {integer} strideFERR - stride for `FERR`
* @param {Float64Array} BERR - backward errors
* @param {integer} strideBERR - stride for `BERR`
* @param {Complex128Array} WORK - complex workspace array
* @param {integer} strideWORK - stride for `WORK`
* @param {Float64Array} RWORK - real workspace array
* @param {integer} strideRWORK - stride for `RWORK`
* @throws {TypeError} First argument must be a valid matrix triangle
* @throws {TypeError} Second argument must be a valid transpose operation
* @throws {TypeError} Third argument must be a valid diagonal type
* @returns {integer} status code (0 = success)
*/
function ztprfs( uplo, trans, diag, N, nrhs, AP, B, LDB, X, LDX, FERR, strideFERR, BERR, strideBERR, WORK, strideWORK, RWORK, strideRWORK ) { // eslint-disable-line max-len, max-params
	var orwork;
	var oberr;
	var oferr;
	var owork;

	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( !isTransposeOperation( trans ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid transpose operation. Value: `%s`.', trans ) );
	}
	if ( !isDiagonalType( diag ) ) {
		throw new TypeError( format( 'invalid argument. Third argument must be a valid diagonal type. Value: `%s`.', diag ) );
	}
	oferr = stride2offset( nrhs, strideFERR );
	oberr = stride2offset( nrhs, strideBERR );
	owork = stride2offset( 2 * N, strideWORK );
	orwork = stride2offset( N, strideRWORK );
	return base( uplo, trans, diag, N, nrhs, AP, 1, 0, B, 1, LDB, 0, X, 1, LDX, 0, FERR, strideFERR, oferr, BERR, strideBERR, oberr, WORK, strideWORK, owork, RWORK, strideRWORK, orwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = ztprfs;
