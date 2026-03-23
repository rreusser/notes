'use strict';

// MODULES //

var ztrtri = require( '../../ztrtri/lib/base.js' );
var zlauum = require( '../../zlauum/lib/base.js' );


// MAIN //

/**
* Computes the inverse of a complex Hermitian positive definite matrix using
* its Cholesky factorization computed by zpotrf.
*
* The inverse is computed by first inverting the triangular Cholesky factor
* (ztrtri), then forming the product of the inverted factor with its
* conjugate transpose (zlauum).
*
* @private
* @param {string} uplo - 'U' for upper triangular factor, 'L' for lower triangular factor
* @param {NonNegativeInteger} N - order of the matrix A
* @param {Complex128Array} A - input/output matrix; on entry, the triangular factor from zpotrf; on exit, the inverse
* @param {integer} strideA1 - stride of the first dimension of A (complex elements)
* @param {integer} strideA2 - stride of the second dimension of A (complex elements)
* @param {NonNegativeInteger} offsetA - starting index for A (complex elements)
* @returns {integer} status code - 0 indicates success, k > 0 indicates the k-th diagonal element of the triangular factor is zero and the matrix is singular
*/
function zpotri( uplo, N, A, strideA1, strideA2, offsetA ) {
	var info;

	// Quick return if possible...
	if ( N === 0 ) {
		return 0;
	}

	// Invert the triangular Cholesky factor...
	info = ztrtri( uplo, 'non-unit', N, A, strideA1, strideA2, offsetA );
	if ( info > 0 ) {
		return info;
	}

	// Form inv(U) * inv(U)^H or inv(L)^H * inv(L)...
	info = zlauum( uplo, N, A, strideA1, strideA2, offsetA );
	return info;
}


// EXPORTS //

module.exports = zpotri;
