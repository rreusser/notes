

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes eigenvalues and Schur decomposition of a real general matrix
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
* @param {Float64Array} WR - input array
* @param {integer} strideWR - stride length for `WR`
* @param {NonNegativeInteger} offsetWR - starting index for `WR`
* @param {Float64Array} WI - input array
* @param {integer} strideWI - stride length for `WI`
* @param {NonNegativeInteger} offsetWI - starting index for `WI`
* @param {Float64Array} VS - input matrix
* @param {integer} strideVS1 - stride of the first dimension of `VS`
* @param {integer} strideVS2 - stride of the second dimension of `VS`
* @param {NonNegativeInteger} offsetVS - starting index for `VS`
* @param {Float64Array} WORK - input array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @param {integer} lwork - lwork
* @param {Float64Array} BWORK - output array
* @param {integer} strideBWORK - stride length for `BWORK`
* @param {NonNegativeInteger} offsetBWORK - starting index for `BWORK`
* @returns {integer} status code (0 = success)
*/
function dgees( jobvs, sort, select, N, A, strideA1, strideA2, offsetA, sdim, WR, strideWR, offsetWR, WI, strideWI, offsetWI, VS, strideVS1, strideVS2, offsetVS, WORK, strideWORK, offsetWORK, lwork, BWORK, strideBWORK, offsetBWORK ) { // eslint-disable-line max-len, max-params
	return base( jobvs, sort, select, N, A, strideA1, strideA2, offsetA, sdim, WR, strideWR, offsetWR, WI, strideWI, offsetWI, VS, strideVS1, strideVS2, offsetVS, WORK, strideWORK, offsetWORK, lwork, BWORK, strideBWORK, offsetBWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dgees;
