

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Estimate the reciprocal condition number of a general matrix
*
* @param {string} norm - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {number} anorm - anorm
* @param {number} rcond - rcond
* @param {Float64Array} WORK - input array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @param {Int32Array} IWORK - output array
* @param {integer} strideIWORK - stride length for `IWORK`
* @param {NonNegativeInteger} offsetIWORK - starting index for `IWORK`
* @returns {integer} status code (0 = success)
*/
function dgecon( norm, N, A, strideA1, strideA2, offsetA, anorm, rcond, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK ) { // eslint-disable-line max-len, max-params
	return base( norm, N, A, strideA1, strideA2, offsetA, anorm, rcond, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dgecon;
