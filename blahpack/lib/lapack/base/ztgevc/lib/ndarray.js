

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Compute eigenvectors of a pair of complex upper triangular matrices
*
* @param {string} side - specifies the operation type
* @param {string} howmny - specifies the operation type
* @param {Float64Array} SELECT - input array
* @param {integer} strideSELECT - stride length for `SELECT`
* @param {NonNegativeInteger} offsetSELECT - starting index for `SELECT`
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} S - input matrix
* @param {integer} strideS1 - stride of the first dimension of `S`
* @param {integer} strideS2 - stride of the second dimension of `S`
* @param {NonNegativeInteger} offsetS - starting index for `S`
* @param {Float64Array} P - input matrix
* @param {integer} strideP1 - stride of the first dimension of `P`
* @param {integer} strideP2 - stride of the second dimension of `P`
* @param {NonNegativeInteger} offsetP - starting index for `P`
* @param {Float64Array} VL - input matrix
* @param {integer} strideVL1 - stride of the first dimension of `VL`
* @param {integer} strideVL2 - stride of the second dimension of `VL`
* @param {NonNegativeInteger} offsetVL - starting index for `VL`
* @param {Float64Array} VR - input matrix
* @param {integer} strideVR1 - stride of the first dimension of `VR`
* @param {integer} strideVR2 - stride of the second dimension of `VR`
* @param {NonNegativeInteger} offsetVR - starting index for `VR`
* @param {integer} mm - mm
* @param {NonNegativeInteger} M - number of rows
* @param {Float64Array} WORK - input array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @param {Float64Array} RWORK - output array
* @param {integer} strideRWORK - stride length for `RWORK`
* @param {NonNegativeInteger} offsetRWORK - starting index for `RWORK`
* @returns {integer} status code (0 = success)
*/
function ztgevc( side, howmny, SELECT, strideSELECT, offsetSELECT, N, S, strideS1, strideS2, offsetS, P, strideP1, strideP2, offsetP, VL, strideVL1, strideVL2, offsetVL, VR, strideVR1, strideVR2, offsetVR, mm, M, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK ) { // eslint-disable-line max-len, max-params
	return base( side, howmny, SELECT, strideSELECT, offsetSELECT, N, S, strideS1, strideS2, offsetS, P, strideP1, strideP2, offsetP, VL, strideVL1, strideVL2, offsetVL, VR, strideVR1, strideVR2, offsetVR, mm, M, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = ztgevc;
