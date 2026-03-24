

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes selected eigenvalues and eigenvectors of a complex Hermitian matrix using MRRR
*
* @param {string} jobz - specifies the operation type
* @param {string} range - specifies the operation type
* @param {string} uplo - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {number} vl - vl
* @param {number} vu - vu
* @param {integer} il - il
* @param {integer} iu - iu
* @param {number} abstol - abstol
* @param {NonNegativeInteger} M - number of rows
* @param {Float64Array} w - input array
* @param {integer} strideW - stride length for `w`
* @param {NonNegativeInteger} offsetW - starting index for `w`
* @param {Float64Array} Z - input matrix
* @param {integer} strideZ1 - stride of the first dimension of `Z`
* @param {integer} strideZ2 - stride of the second dimension of `Z`
* @param {NonNegativeInteger} offsetZ - starting index for `Z`
* @param {Int32Array} ISUPPZ - input array
* @param {integer} strideISUPPZ - stride length for `ISUPPZ`
* @param {NonNegativeInteger} offsetISUPPZ - starting index for `ISUPPZ`
* @param {Float64Array} WORK - input array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @param {integer} lwork - lwork
* @param {Float64Array} RWORK - input array
* @param {integer} strideRWORK - stride length for `RWORK`
* @param {NonNegativeInteger} offsetRWORK - starting index for `RWORK`
* @param {integer} lrwork - lrwork
* @param {Int32Array} IWORK - output array
* @param {integer} strideIWORK - stride length for `IWORK`
* @param {NonNegativeInteger} offsetIWORK - starting index for `IWORK`
* @param {integer} liwork - liwork
* @returns {integer} status code (0 = success)
*/
function zheevr( jobz, range, uplo, N, A, strideA1, strideA2, offsetA, vl, vu, il, iu, abstol, M, w, strideW, offsetW, Z, strideZ1, strideZ2, offsetZ, ISUPPZ, strideISUPPZ, offsetISUPPZ, WORK, strideWORK, offsetWORK, lwork, RWORK, strideRWORK, offsetRWORK, lrwork, IWORK, strideIWORK, offsetIWORK, liwork ) { // eslint-disable-line max-len, max-params
	return base( jobz, range, uplo, N, A, strideA1, strideA2, offsetA, vl, vu, il, iu, abstol, M, w, strideW, offsetW, Z, strideZ1, strideZ2, offsetZ, ISUPPZ, strideISUPPZ, offsetISUPPZ, WORK, strideWORK, offsetWORK, lwork, RWORK, strideRWORK, offsetRWORK, lrwork, IWORK, strideIWORK, offsetIWORK, liwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zheevr;
