'use strict';

// MODULES //

var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var real = require( '@stdlib/complex/float64/real' );
var zdotc = require( '../../../../blas/base/zdotc/lib/base.js' );
var zgemv = require( '../../../../blas/base/zgemv/lib/base.js' );
var zdscal = require( '../../../../blas/base/zdscal/lib/base.js' );
var zlacgv = require( '../../zlacgv/lib/base.js' );


// VARIABLES //

var CONE = new Complex128( 1.0, 0.0 );
var CMONE = new Complex128( -1.0, 0.0 );


// MAIN //

/**
* Computes the Cholesky factorization of a complex Hermitian positive definite
* matrix A using the unblocked algorithm (Level 2 BLAS).
*
* The factorization has the form:
*   A = U^H * U,  if uplo = 'upper', or
*   A = L * L^H,  if uplo = 'lower',
* where U is upper triangular and L is lower triangular.
*
* @private
* @param {string} uplo - specifies whether upper or lower triangle is stored ('U' or 'L')
* @param {NonNegativeInteger} N - order of matrix A
* @param {Complex128Array} A - input/output Hermitian matrix
* @param {integer} strideA1 - stride of the first dimension of A (in complex elements)
* @param {integer} strideA2 - stride of the second dimension of A (in complex elements)
* @param {NonNegativeInteger} offsetA - starting index for A (in complex elements)
* @returns {integer} info - 0 if successful, k>0 if the leading minor of order k is not positive definite
*/
function zpotf2( uplo, N, A, strideA1, strideA2, offsetA ) {
	var sa1;
	var sa2;
	var Av;
	var oA;
	var da;
	var ajj;
	var j;

	if ( N === 0 ) {
		return 0;
	}

	Av = reinterpret( A, 0 );
	oA = offsetA * 2;
	sa1 = strideA1 * 2;
	sa2 = strideA2 * 2;

	if ( uplo === 'upper' ) {
		// Compute the Cholesky factorization A = U^H * U.
		for ( j = 0; j < N; j++ ) {
			// Compute U(j,j) and test for non-positive-definiteness.
			// AJJ = DBLE(A(J,J)) - DBLE(ZDOTC(J-1, A(1,J), 1, A(1,J), 1))
			da = oA + j * sa1 + j * sa2;
			ajj = Av[ da ] - real( zdotc( j, A, strideA1, offsetA + j * strideA2, A, strideA1, offsetA + j * strideA2 ) );

			if ( ajj <= 0.0 || ajj !== ajj ) {
				Av[ da ] = ajj;
				Av[ da + 1 ] = 0.0;
				return j + 1;
			}
			ajj = Math.sqrt( ajj );
			Av[ da ] = ajj;
			Av[ da + 1 ] = 0.0;

			// Compute elements j+1:N-1 of row j.
			if ( j < N - 1 ) {
				// ZLACGV(J-1, A(1,J), 1)
				zlacgv( j, A, strideA1, offsetA + j * strideA2 );
				// ZGEMV('Transpose', J-1, N-J, -CONE, A(1,J+1), LDA, A(1,J), 1, CONE, A(J,J+1), LDA)
				zgemv( 'transpose', j, N - j - 1, CMONE,
					A, strideA1, strideA2, offsetA + ( j + 1 ) * strideA2,
					A, strideA1, offsetA + j * strideA2,
					CONE,
					A, strideA2, offsetA + j * strideA1 + ( j + 1 ) * strideA2
				);
				// ZLACGV(J-1, A(1,J), 1) -- undo conjugation
				zlacgv( j, A, strideA1, offsetA + j * strideA2 );
				// ZDSCAL(N-J, 1/AJJ, A(J,J+1), LDA)
				zdscal( N - j - 1, 1.0 / ajj,
					A, strideA2, offsetA + j * strideA1 + ( j + 1 ) * strideA2
				);
			}
		}
	} else {
		// Compute the Cholesky factorization A = L * L^H.
		for ( j = 0; j < N; j++ ) {
			// Compute L(j,j) and test for non-positive-definiteness.
			// AJJ = DBLE(A(J,J)) - DBLE(ZDOTC(J-1, A(J,1), LDA, A(J,1), LDA))
			da = oA + j * sa1 + j * sa2;
			ajj = Av[ da ] - real( zdotc( j, A, strideA2, offsetA + j * strideA1, A, strideA2, offsetA + j * strideA1 ) );

			if ( ajj <= 0.0 || ajj !== ajj ) {
				Av[ da ] = ajj;
				Av[ da + 1 ] = 0.0;
				return j + 1;
			}
			ajj = Math.sqrt( ajj );
			Av[ da ] = ajj;
			Av[ da + 1 ] = 0.0;

			// Compute elements j+1:N-1 of column j.
			if ( j < N - 1 ) {
				// ZLACGV(J-1, A(J,1), LDA)
				zlacgv( j, A, strideA2, offsetA + j * strideA1 );
				// ZGEMV('No transpose', N-J, J-1, -CONE, A(J+1,1), LDA, A(J,1), LDA, CONE, A(J+1,J), 1)
				zgemv( 'no-transpose', N - j - 1, j, CMONE,
					A, strideA1, strideA2, offsetA + ( j + 1 ) * strideA1,
					A, strideA2, offsetA + j * strideA1,
					CONE,
					A, strideA1, offsetA + ( j + 1 ) * strideA1 + j * strideA2
				);
				// ZLACGV(J-1, A(J,1), LDA) -- undo conjugation
				zlacgv( j, A, strideA2, offsetA + j * strideA1 );
				// ZDSCAL(N-J, 1/AJJ, A(J+1,J), 1)
				zdscal( N - j - 1, 1.0 / ajj,
					A, strideA1, offsetA + ( j + 1 ) * strideA1 + j * strideA2
				);
			}
		}
	}
	return 0;
}


// EXPORTS //

module.exports = zpotf2;
