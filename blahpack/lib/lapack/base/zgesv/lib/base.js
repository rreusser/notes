'use strict';

// MODULES //

var zgetrf = require( '../../zgetrf/lib/base.js' );
var zgetrs = require( '../../zgetrs/lib/base.js' );


// MAIN //

/**
* Computes the solution to a complex system of linear equations A * X = B,
* where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
*
* The LU decomposition with partial pivoting and row interchanges is
* used to factor A as A = P * L * U, then the factored form of A is
* used to solve the system of equations A * X = B.
*
* IPIV is an output array that receives 0-based pivot indices from zgetrf.
*
* @private
* @param {NonNegativeInteger} N - order of matrix A (number of rows and columns)
* @param {NonNegativeInteger} nrhs - number of right-hand side columns
* @param {Complex128Array} A - input/output N-by-N matrix; on exit, L and U factors
* @param {integer} strideA1 - stride of the first dimension of A
* @param {integer} strideA2 - stride of the second dimension of A
* @param {NonNegativeInteger} offsetA - index offset for A
* @param {Int32Array} IPIV - output pivot indices (0-based), length N
* @param {integer} strideIPIV - stride for IPIV
* @param {NonNegativeInteger} offsetIPIV - index offset for IPIV
* @param {Complex128Array} B - input/output N-by-NRHS matrix; on exit, the solution X
* @param {integer} strideB1 - stride of the first dimension of B
* @param {integer} strideB2 - stride of the second dimension of B
* @param {NonNegativeInteger} offsetB - index offset for B
* @returns {integer} info - 0 if successful, k if U(k-1,k-1) is exactly zero
*/
function zgesv( N, nrhs, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV, B, strideB1, strideB2, offsetB ) { // eslint-disable-line max-len, max-params
	var info;

	if ( N === 0 || nrhs === 0 ) {
		return 0;
	}

	// Compute the LU factorization of A
	info = zgetrf( N, N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV );

	if ( info === 0 ) {
		// Solve the system using the factorization
		zgetrs( 'N', N, nrhs, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV, B, strideB1, strideB2, offsetB );
	}

	return info;
}


// EXPORTS //

module.exports = zgesv;
