

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes all eigenvalues and optionally eigenvectors of a complex generalized Hermitian-definite eigenproblem
*
* @param {integer} itype - itype
* @param {string} jobz - specifies the operation type
* @param {string} uplo - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} B - input matrix
* @param {integer} strideB1 - stride of the first dimension of `B`
* @param {integer} strideB2 - stride of the second dimension of `B`
* @param {NonNegativeInteger} offsetB - starting index for `B`
* @param {Float64Array} w - input array
* @param {integer} strideW - stride length for `w`
* @param {NonNegativeInteger} offsetW - starting index for `w`
* @param {Float64Array} WORK - input array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @param {integer} lwork - lwork
* @param {Float64Array} RWORK - output array
* @param {integer} strideRWORK - stride length for `RWORK`
* @param {NonNegativeInteger} offsetRWORK - starting index for `RWORK`
* @returns {integer} status code (0 = success)
*/
function zhegv( itype, jobz, uplo, N, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, w, strideW, offsetW, WORK, strideWORK, offsetWORK, lwork, RWORK, strideRWORK, offsetRWORK ) { // eslint-disable-line max-len, max-params
	return base( itype, jobz, uplo, N, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, w, strideW, offsetW, WORK, strideWORK, offsetWORK, lwork, RWORK, strideRWORK, offsetRWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zhegv;
