

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Reorder Schur factorization and compute condition numbers
*
* @param {string} job - specifies the operation type
* @param {string} compq - specifies the operation type
* @param {Float64Array} SELECT - input array
* @param {integer} strideSELECT - stride length for `SELECT`
* @param {NonNegativeInteger} offsetSELECT - starting index for `SELECT`
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} T - input matrix
* @param {integer} strideT1 - stride of the first dimension of `T`
* @param {integer} strideT2 - stride of the second dimension of `T`
* @param {NonNegativeInteger} offsetT - starting index for `T`
* @param {Float64Array} Q - input matrix
* @param {integer} strideQ1 - stride of the first dimension of `Q`
* @param {integer} strideQ2 - stride of the second dimension of `Q`
* @param {NonNegativeInteger} offsetQ - starting index for `Q`
* @param {Float64Array} w - input array
* @param {integer} strideW - stride length for `w`
* @param {NonNegativeInteger} offsetW - starting index for `w`
* @param {NonNegativeInteger} M - number of rows
* @param {number} s - s
* @param {number} sep - sep
* @param {Float64Array} WORK - output array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @param {integer} lwork - lwork
* @returns {integer} status code (0 = success)
*/
function ztrsen( job, compq, SELECT, strideSELECT, offsetSELECT, N, T, strideT1, strideT2, offsetT, Q, strideQ1, strideQ2, offsetQ, w, strideW, offsetW, M, s, sep, WORK, strideWORK, offsetWORK, lwork ) { // eslint-disable-line max-len, max-params
	return base( job, compq, SELECT, strideSELECT, offsetSELECT, N, T, strideT1, strideT2, offsetT, Q, strideQ1, strideQ2, offsetQ, w, strideW, offsetW, M, s, sep, WORK, strideWORK, offsetWORK, lwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = ztrsen;
