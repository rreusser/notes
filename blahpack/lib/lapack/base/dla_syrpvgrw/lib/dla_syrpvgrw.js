

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes the reciprocal pivot growth factor `norm(A)/norm(U)` for a symmetric indefinite matrix.
*
* @param {string} uplo - specifies whether the upper or lower triangle is stored (`upper` or `lower`)
* @param {NonNegativeInteger} N - number of rows and columns of the matrix A
* @param {NonNegativeInteger} info - value of INFO returned from dsytrf (0 = success, k > 0 = singular at column k, 1-based)
* @param {Float64Array} A - input matrix A (column-major)
* @param {NonNegativeInteger} LDA - leading dimension of `A`
* @param {Float64Array} AF - factored matrix from dsytrf (column-major)
* @param {NonNegativeInteger} LDAF - leading dimension of `AF`
* @param {Int32Array} IPIV - pivot indices from dsytrf (0-based)
* @param {Float64Array} WORK - workspace array of length at least `2*N`
* @returns {number} reciprocal pivot growth factor
*/
function dla_syrpvgrw( uplo, N, info, A, LDA, AF, LDAF, IPIV, WORK ) { // eslint-disable-line max-len, max-params
	return base( uplo, N, info, A, 1, LDA, 0, AF, 1, LDAF, 0, IPIV, 1, 0, WORK, 1, 0 );
}


// EXPORTS //

module.exports = dla_syrpvgrw;
