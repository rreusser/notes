

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Apply a real Householder reflector to a matrix.
*
* @param {string} side - specifies the operation type
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} v - input array
* @param {integer} strideV - stride length for `v`
* @param {NonNegativeInteger} offsetV - starting index for `v`
* @param {number} tau - tau
* @param {Float64Array} C - input matrix
* @param {integer} strideC1 - stride of the first dimension of `C`
* @param {integer} strideC2 - stride of the second dimension of `C`
* @param {NonNegativeInteger} offsetC - starting index for `C`
* @param {Float64Array} WORK - output array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
*/
function dlarf( side, M, N, v, strideV, offsetV, tau, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	return base( side, M, N, v, strideV, offsetV, tau, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlarf;
