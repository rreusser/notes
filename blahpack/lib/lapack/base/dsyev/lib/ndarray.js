

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Compute eigenvalues and optionally eigenvectors of a real symmetric matrix
*
* @param {string} jobz - specifies the operation type
* @param {string} uplo - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} w - input array
* @param {integer} strideW - stride length for `w`
* @param {NonNegativeInteger} offsetW - starting index for `w`
* @param {Float64Array} WORK - output array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @param {integer} lwork - lwork
* @returns {integer} status code (0 = success)
*/
function dsyev( jobz, uplo, N, A, strideA1, strideA2, offsetA, w, strideW, offsetW, WORK, strideWORK, offsetWORK, lwork ) { // eslint-disable-line max-len, max-params
	return base( jobz, uplo, N, A, strideA1, strideA2, offsetA, w, strideW, offsetW, WORK, strideWORK, offsetWORK, lwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dsyev;
