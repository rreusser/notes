

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Reduce a complex matrix to bidiagonal form (unblocked)
*
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} d - input array
* @param {integer} strideD - stride length for `d`
* @param {NonNegativeInteger} offsetD - starting index for `d`
* @param {Float64Array} e - input array
* @param {integer} strideE - stride length for `e`
* @param {NonNegativeInteger} offsetE - starting index for `e`
* @param {Float64Array} TAUQ - input array
* @param {integer} strideTAUQ - stride length for `TAUQ`
* @param {NonNegativeInteger} offsetTAUQ - starting index for `TAUQ`
* @param {Float64Array} TAUP - input array
* @param {integer} strideTAUP - stride length for `TAUP`
* @param {NonNegativeInteger} offsetTAUP - starting index for `TAUP`
* @param {Float64Array} WORK - output array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @returns {integer} status code (0 = success)
*/
function zgebd2( M, N, A, strideA1, strideA2, offsetA, d, strideD, offsetD, e, strideE, offsetE, TAUQ, strideTAUQ, offsetTAUQ, TAUP, strideTAUP, offsetTAUP, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	return base( M, N, A, strideA1, strideA2, offsetA, d, strideD, offsetD, e, strideE, offsetE, TAUQ, strideTAUQ, offsetTAUQ, TAUP, strideTAUP, offsetTAUP, WORK, strideWORK, offsetWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zgebd2;
