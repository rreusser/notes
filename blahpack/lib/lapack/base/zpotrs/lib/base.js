'use strict';

// MODULES //

var Complex128 = require( '@stdlib/complex/float64/ctor' );
var ztrsm = require( '../../../../blas/base/ztrsm/lib/base.js' );


// VARIABLES //

var CONE = new Complex128( 1.0, 0.0 );


// MAIN //

/**
* Solves a system of linear equations A*X = B with a Hermitian positive
* definite matrix A using the Cholesky factorization A = U^H*U or A = L*L^H
* computed by zpotrf.
*
* @private
* @param {string} uplo - 'U' if upper Cholesky factor stored, 'L' if lower
* @param {NonNegativeInteger} N - order of matrix A
* @param {NonNegativeInteger} nrhs - number of right-hand side columns
* @param {Complex128Array} A - the Cholesky factor (from zpotrf)
* @param {integer} strideA1 - stride of the first dimension of A (in complex elements)
* @param {integer} strideA2 - stride of the second dimension of A (in complex elements)
* @param {NonNegativeInteger} offsetA - index offset for A (in complex elements)
* @param {Complex128Array} B - right-hand side matrix, overwritten with solution
* @param {integer} strideB1 - stride of the first dimension of B (in complex elements)
* @param {integer} strideB2 - stride of the second dimension of B (in complex elements)
* @param {NonNegativeInteger} offsetB - index offset for B (in complex elements)
* @returns {integer} info - 0 if successful
*/
function zpotrs( uplo, N, nrhs, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB ) { // eslint-disable-line max-len, max-params
	if ( N === 0 || nrhs === 0 ) {
		return 0;
	}

	if ( uplo === 'U' || uplo === 'u' ) {
		// Solve A*X = B where A = U^H*U.

		// Solve U^H * Y = B (forward substitution with conjugate transpose)
		ztrsm( 'L', 'U', 'C', 'N', N, nrhs, CONE,
			A, strideA1, strideA2, offsetA,
			B, strideB1, strideB2, offsetB
		);

		// Solve U * X = Y (back substitution)
		ztrsm( 'L', 'U', 'N', 'N', N, nrhs, CONE,
			A, strideA1, strideA2, offsetA,
			B, strideB1, strideB2, offsetB
		);
	} else {
		// Solve A*X = B where A = L*L^H.

		// Solve L * Y = B (forward substitution)
		ztrsm( 'L', 'L', 'N', 'N', N, nrhs, CONE,
			A, strideA1, strideA2, offsetA,
			B, strideB1, strideB2, offsetB
		);

		// Solve L^H * X = Y (back substitution with conjugate transpose)
		ztrsm( 'L', 'L', 'C', 'N', N, nrhs, CONE,
			A, strideA1, strideA2, offsetA,
			B, strideB1, strideB2, offsetB
		);
	}

	return 0;
}


// EXPORTS //

module.exports = zpotrs;
