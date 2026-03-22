

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Balance a pair of complex general matrices for the generalized eigenvalue problem.
*
* @param {string} job - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} B - input matrix
* @param {integer} strideB1 - stride of the first dimension of `B`
* @param {integer} strideB2 - stride of the second dimension of `B`
* @param {NonNegativeInteger} offsetB - starting index for `B`
* @param {Float64Array} LSCALE - input array
* @param {integer} strideLSCALE - stride length for `LSCALE`
* @param {NonNegativeInteger} offsetLSCALE - starting index for `LSCALE`
* @param {Float64Array} RSCALE - input array
* @param {integer} strideRSCALE - stride length for `RSCALE`
* @param {NonNegativeInteger} offsetRSCALE - starting index for `RSCALE`
* @param {Float64Array} WORK - output array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @returns {Object} result with properties: info (0=success), ilo (1-based), ihi (1-based)
*/
function zggbal( job, N, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, LSCALE, strideLSCALE, offsetLSCALE, RSCALE, strideRSCALE, offsetRSCALE, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	return base( job, N, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, LSCALE, strideLSCALE, offsetLSCALE, RSCALE, strideRSCALE, offsetRSCALE, WORK, strideWORK, offsetWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zggbal;
