

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Compute the SVD of a complex matrix
*
* @param {string} jobu - specifies the operation type
* @param {string} jobvt - specifies the operation type
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} s - input array
* @param {integer} strideS - stride length for `s`
* @param {NonNegativeInteger} offsetS - starting index for `s`
* @param {Float64Array} U - input matrix
* @param {integer} strideU1 - stride of the first dimension of `U`
* @param {integer} strideU2 - stride of the second dimension of `U`
* @param {NonNegativeInteger} offsetU - starting index for `U`
* @param {Float64Array} VT - input matrix
* @param {integer} strideVT1 - stride of the first dimension of `VT`
* @param {integer} strideVT2 - stride of the second dimension of `VT`
* @param {NonNegativeInteger} offsetVT - starting index for `VT`
* @param {Float64Array} WORK - input array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @param {integer} lwork - lwork
* @param {Float64Array} RWORK - output array
* @param {integer} strideRWORK - stride length for `RWORK`
* @param {NonNegativeInteger} offsetRWORK - starting index for `RWORK`
* @returns {integer} status code (0 = success)
*/
function zgesvd( jobu, jobvt, M, N, A, strideA1, strideA2, offsetA, s, strideS, offsetS, U, strideU1, strideU2, offsetU, VT, strideVT1, strideVT2, offsetVT, WORK, strideWORK, offsetWORK, lwork, RWORK, strideRWORK, offsetRWORK ) { // eslint-disable-line max-len, max-params
	return base( jobu, jobvt, M, N, A, strideA1, strideA2, offsetA, s, strideS, offsetS, U, strideU1, strideU2, offsetU, VT, strideVT1, strideVT2, offsetVT, WORK, strideWORK, offsetWORK, lwork, RWORK, strideRWORK, offsetRWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zgesvd;
