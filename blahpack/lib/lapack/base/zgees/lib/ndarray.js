

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Compute eigenvalues and Schur decomposition of a complex matrix
*
* @param {string} jobvs - specifies the operation type
* @param {string} sort - specifies the operation type
* @param {boolean} select - select
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {integer} sdim - sdim
* @param {Float64Array} w - input array
* @param {integer} strideW - stride length for `w`
* @param {NonNegativeInteger} offsetW - starting index for `w`
* @param {Float64Array} VS - input matrix
* @param {integer} strideVS1 - stride of the first dimension of `VS`
* @param {integer} strideVS2 - stride of the second dimension of `VS`
* @param {NonNegativeInteger} offsetVS - starting index for `VS`
* @param {Float64Array} WORK - input array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @param {integer} lwork - lwork
* @param {Float64Array} RWORK - input array
* @param {integer} strideRWORK - stride length for `RWORK`
* @param {NonNegativeInteger} offsetRWORK - starting index for `RWORK`
* @param {Float64Array} BWORK - output array
* @param {integer} strideBWORK - stride length for `BWORK`
* @param {NonNegativeInteger} offsetBWORK - starting index for `BWORK`
* @returns {integer} status code (0 = success)
*/
function zgees( jobvs, sort, select, N, A, strideA1, strideA2, offsetA, sdim, w, strideW, offsetW, VS, strideVS1, strideVS2, offsetVS, WORK, strideWORK, offsetWORK, lwork, RWORK, strideRWORK, offsetRWORK, BWORK, strideBWORK, offsetBWORK ) { // eslint-disable-line max-len, max-params
	return base( jobvs, sort, select, N, A, strideA1, strideA2, offsetA, sdim, w, strideW, offsetW, VS, strideVS1, strideVS2, offsetVS, WORK, strideWORK, offsetWORK, lwork, RWORK, strideRWORK, offsetRWORK, BWORK, strideBWORK, offsetBWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zgees;
