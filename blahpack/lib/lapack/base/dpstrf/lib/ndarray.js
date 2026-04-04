
'use strict';

// MODULES //

var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Computes the Cholesky factorization with complete pivoting of a real symmetric positive semi-definite matrix (blocked algorithm).
*
* @param {string} uplo - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Int32Array} piv - input array
* @param {integer} stridePIV - stride length for `piv`
* @param {NonNegativeInteger} offsetPIV - starting index for `piv`
* @param {integer} rank - rank
* @param {number} tol - tol
* @param {number} work - work
* @throws {TypeError} First argument must be a valid matrix triangle
* @returns {integer} status code (0 = success)
*/
function dpstrf( uplo, N, A, strideA1, strideA2, offsetA, piv, stridePIV, offsetPIV, rank, tol, work ) { // eslint-disable-line max-len, max-params
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	return base( uplo, N, A, strideA1, strideA2, offsetA, piv, stridePIV, offsetPIV, rank, tol, work ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dpstrf;
