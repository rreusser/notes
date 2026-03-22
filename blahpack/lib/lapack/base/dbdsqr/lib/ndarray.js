

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Compute the SVD of a real bidiagonal matrix
*
* @param {string} uplo - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {integer} ncvt - ncvt
* @param {integer} nru - nru
* @param {integer} ncc - ncc
* @param {Float64Array} d - input array
* @param {integer} strideD - stride length for `d`
* @param {NonNegativeInteger} offsetD - starting index for `d`
* @param {Float64Array} e - input array
* @param {integer} strideE - stride length for `e`
* @param {NonNegativeInteger} offsetE - starting index for `e`
* @param {Float64Array} VT - input matrix
* @param {integer} strideVT1 - stride of the first dimension of `VT`
* @param {integer} strideVT2 - stride of the second dimension of `VT`
* @param {NonNegativeInteger} offsetVT - starting index for `VT`
* @param {Float64Array} U - input matrix
* @param {integer} strideU1 - stride of the first dimension of `U`
* @param {integer} strideU2 - stride of the second dimension of `U`
* @param {NonNegativeInteger} offsetU - starting index for `U`
* @param {Float64Array} C - input matrix
* @param {integer} strideC1 - stride of the first dimension of `C`
* @param {integer} strideC2 - stride of the second dimension of `C`
* @param {NonNegativeInteger} offsetC - starting index for `C`
* @param {Float64Array} WORK - output array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @returns {integer} status code (0 = success)
*/
function dbdsqr( uplo, N, ncvt, nru, ncc, d, strideD, offsetD, e, strideE, offsetE, VT, strideVT1, strideVT2, offsetVT, U, strideU1, strideU2, offsetU, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	return base( uplo, N, ncvt, nru, ncc, d, strideD, offsetD, e, strideE, offsetE, VT, strideVT1, strideVT2, offsetVT, U, strideU1, strideU2, offsetU, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dbdsqr;
