'use strict';

// MODULES //

var dlaswp = require( '../../dlaswp/lib/base.js' );
var dtrsm = require( '../../../../blas/base/dtrsm/lib/base.js' );


// MAIN //

/**
* Solves a system of linear equations A*X = B or A^T*X = B with a general
* N-by-N matrix A using the LU factorization computed by dgetrf/dgetrf2.
*
* IPIV must contain 0-based pivot indices (as produced by dgetrf/dgetrf2).
*
* @private
* @param {string} trans - 'N' for no transpose, 'T' or 'C' for transpose
* @param {NonNegativeInteger} N - order of matrix A
* @param {NonNegativeInteger} nrhs - number of right-hand side columns
* @param {Float64Array} A - LU-factored N-by-N matrix (from dgetrf)
* @param {integer} strideA1 - stride of the first dimension of A
* @param {integer} strideA2 - stride of the second dimension of A
* @param {NonNegativeInteger} offsetA - index offset for A
* @param {Int32Array} IPIV - pivot indices from dgetrf (0-based)
* @param {integer} strideIPIV - stride for IPIV
* @param {NonNegativeInteger} offsetIPIV - index offset for IPIV
* @param {Float64Array} B - right-hand side matrix, overwritten with solution
* @param {integer} strideB1 - stride of the first dimension of B
* @param {integer} strideB2 - stride of the second dimension of B
* @param {NonNegativeInteger} offsetB - index offset for B
* @returns {integer} info - 0 if successful
*/
function dgetrs( trans, N, nrhs, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV, B, strideB1, strideB2, offsetB ) { // eslint-disable-line max-len, max-params
	if ( N === 0 || nrhs === 0 ) {
		return 0;
	}

	if ( trans === 'N' || trans === 'n' ) {
		// Solve A * X = B.

		// Apply row interchanges to the right-hand sides.
		// dlaswp uses 0-based k1, k2
		dlaswp( nrhs, B, strideB1, strideB2, offsetB, 0, N - 1, IPIV, strideIPIV, offsetIPIV, 1 );

		// Solve L * Y = P * B (forward substitution, L is unit lower triangular)
		dtrsm( 'L', 'L', 'N', 'U', N, nrhs, 1.0,
			A, strideA1, strideA2, offsetA,
			B, strideB1, strideB2, offsetB
		);

		// Solve U * X = Y (back substitution)
		dtrsm( 'L', 'U', 'N', 'N', N, nrhs, 1.0,
			A, strideA1, strideA2, offsetA,
			B, strideB1, strideB2, offsetB
		);
	} else {
		// Solve A^T * X = B.

		// Solve U^T * Y = B (forward substitution with U transposed)
		dtrsm( 'L', 'U', 'T', 'N', N, nrhs, 1.0,
			A, strideA1, strideA2, offsetA,
			B, strideB1, strideB2, offsetB
		);

		// Solve L^T * X = Y (back substitution with L transposed, unit diagonal)
		dtrsm( 'L', 'L', 'T', 'U', N, nrhs, 1.0,
			A, strideA1, strideA2, offsetA,
			B, strideB1, strideB2, offsetB
		);

		// Apply row interchanges in reverse order
		dlaswp( nrhs, B, strideB1, strideB2, offsetB, N - 1, 0, IPIV, strideIPIV, offsetIPIV, -1 );
	}

	return 0;
}


// EXPORTS //

module.exports = dgetrs;
