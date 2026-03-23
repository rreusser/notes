'use strict';

// MODULES //

var Complex128 = require( '@stdlib/complex/float64/ctor' );
var zlauu2 = require( '../../zlauu2/lib/base.js' );
var zgemm = require( '../../../../blas/base/zgemm/lib/base.js' );
var zherk = require( '../../../../blas/base/zherk/lib/base.js' );
var ztrmm = require( '../../../../blas/base/ztrmm/lib/base.js' );


// VARIABLES //

var NB = 2; // Block size for blocked algorithm (matches ztrtri)
var CONE = new Complex128( 1.0, 0.0 );


// MAIN //

/**
* Computes the product of a complex triangular matrix with its conjugate
* transpose (blocked algorithm).
*
* If UPLO = 'U', computes U * U^H (upper triangle of result stored in A).
* If UPLO = 'L', computes L^H * L (lower triangle of result stored in A).
*
* On exit, the upper (or lower) triangle of A is overwritten with the
* upper (or lower) triangle of the product.
*
* @private
* @param {string} uplo - 'U' for upper triangular, 'L' for lower triangular
* @param {NonNegativeInteger} N - order of the triangular matrix
* @param {Complex128Array} A - input/output triangular matrix
* @param {integer} strideA1 - stride of the first dimension of A (complex elements)
* @param {integer} strideA2 - stride of the second dimension of A (complex elements)
* @param {NonNegativeInteger} offsetA - starting index for A (complex elements)
* @returns {integer} info - 0 if successful
*/
function zlauum( uplo, N, A, strideA1, strideA2, offsetA ) {
	var upper;
	var sa1;
	var sa2;
	var ib;
	var i;

	if ( N === 0 ) {
		return 0;
	}

	upper = ( uplo === 'U' || uplo === 'u' );
	sa1 = strideA1;
	sa2 = strideA2;

	// Use unblocked code for small matrices
	if ( NB <= 1 || NB >= N ) {
		return zlauu2( uplo, N, A, sa1, sa2, offsetA );
	}

	// Blocked algorithm
	if ( upper ) {
		// Compute U * U^H by blocks
		for ( i = 0; i < N; i += NB ) {
			ib = Math.min( NB, N - i );

			// Update the leading i rows of the current block column:
			// A(0:i-1, i:i+ib-1) := A(0:i-1, i:i+ib-1) * U(i:i+ib-1, i:i+ib-1)^H
			ztrmm( 'R', 'U', 'C', 'N', i, ib, CONE,
				A, sa1, sa2, offsetA + i * sa1 + i * sa2,
				A, sa1, sa2, offsetA + i * sa2 );

			// Compute the product of the diagonal block:
			// A(i:i+ib-1, i:i+ib-1) := U(i:i+ib-1, i:i+ib-1) * U(i:i+ib-1, i:i+ib-1)^H
			zlauu2( 'U', ib, A, sa1, sa2, offsetA + i * sa1 + i * sa2 );

			if ( i + ib < N ) {
				// Update the leading i rows using remaining columns:
				// A(0:i-1, i:i+ib-1) += A(0:i-1, i+ib:N-1) * A(i:i+ib-1, i+ib:N-1)^H
				zgemm( 'N', 'C', i, ib, N - i - ib, CONE,
					A, sa1, sa2, offsetA + ( i + ib ) * sa2,
					A, sa1, sa2, offsetA + i * sa1 + ( i + ib ) * sa2,
					CONE,
					A, sa1, sa2, offsetA + i * sa2 );

				// Rank-ib update of diagonal block:
				// A(i:i+ib-1, i:i+ib-1) += A(i:i+ib-1, i+ib:N-1) * A(i:i+ib-1, i+ib:N-1)^H
				zherk( 'U', 'N', ib, N - i - ib, 1.0,
					A, sa1, sa2, offsetA + i * sa1 + ( i + ib ) * sa2,
					1.0,
					A, sa1, sa2, offsetA + i * sa1 + i * sa2 );
			}
		}
	} else {
		// Compute L^H * L by blocks
		for ( i = 0; i < N; i += NB ) {
			ib = Math.min( NB, N - i );

			// Update the leading i columns of the current block row:
			// A(i:i+ib-1, 0:i-1) := L(i:i+ib-1, i:i+ib-1)^H * A(i:i+ib-1, 0:i-1)
			ztrmm( 'L', 'L', 'C', 'N', ib, i, CONE,
				A, sa1, sa2, offsetA + i * sa1 + i * sa2,
				A, sa1, sa2, offsetA + i * sa1 );

			// Compute the product of the diagonal block:
			// A(i:i+ib-1, i:i+ib-1) := L(i:i+ib-1, i:i+ib-1)^H * L(i:i+ib-1, i:i+ib-1)
			zlauu2( 'L', ib, A, sa1, sa2, offsetA + i * sa1 + i * sa2 );

			if ( i + ib < N ) {
				// Update the leading i columns using remaining rows:
				// A(i:i+ib-1, 0:i-1) += A(i+ib:N-1, i:i+ib-1)^H * A(i+ib:N-1, 0:i-1)
				zgemm( 'C', 'N', ib, i, N - i - ib, CONE,
					A, sa1, sa2, offsetA + ( i + ib ) * sa1 + i * sa2,
					A, sa1, sa2, offsetA + ( i + ib ) * sa1,
					CONE,
					A, sa1, sa2, offsetA + i * sa1 );

				// Rank-ib update of diagonal block:
				// A(i:i+ib-1, i:i+ib-1) += A(i+ib:N-1, i:i+ib-1)^H * A(i+ib:N-1, i:i+ib-1)
				zherk( 'L', 'C', ib, N - i - ib, 1.0,
					A, sa1, sa2, offsetA + ( i + ib ) * sa1 + i * sa2,
					1.0,
					A, sa1, sa2, offsetA + i * sa1 + i * sa2 );
			}
		}
	}
	return 0;
}


// EXPORTS //

module.exports = zlauum;
