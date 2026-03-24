

'use strict';

// MAIN //

/**
* Computes eigenvalues and eigenvectors of a real general matrix
*
* @private
* @param {string} jobvl - specifies the operation type
* @param {string} jobvr - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} WR - input array
* @param {integer} strideWR - stride length for `WR`
* @param {NonNegativeInteger} offsetWR - starting index for `WR`
* @param {Float64Array} WI - input array
* @param {integer} strideWI - stride length for `WI`
* @param {NonNegativeInteger} offsetWI - starting index for `WI`
* @param {Float64Array} VL - input matrix
* @param {integer} strideVL1 - stride of the first dimension of `VL`
* @param {integer} strideVL2 - stride of the second dimension of `VL`
* @param {NonNegativeInteger} offsetVL - starting index for `VL`
* @param {Float64Array} VR - input matrix
* @param {integer} strideVR1 - stride of the first dimension of `VR`
* @param {integer} strideVR2 - stride of the second dimension of `VR`
* @param {NonNegativeInteger} offsetVR - starting index for `VR`
* @param {Float64Array} WORK - output array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @param {integer} lwork - lwork
* @returns {integer} status code (0 = success)
*/
function dgeev( jobvl, jobvr, N, A, strideA1, strideA2, offsetA, WR, strideWR, offsetWR, WI, strideWI, offsetWI, VL, strideVL1, strideVL2, offsetVL, VR, strideVR1, strideVR2, offsetVR, WORK, strideWORK, offsetWORK, lwork ) { // eslint-disable-line max-len, max-params
	// TODO: implement
	throw new Error( 'not yet implemented' );
}


// EXPORTS //

module.exports = dgeev;
