

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Apply a real block Householder reflector to a matrix.
*
* @param {string} side - specifies the operation type
* @param {string} trans - specifies the operation type
* @param {string} direct - specifies the operation type
* @param {string} storev - specifies the operation type
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {NonNegativeInteger} K - number of superdiagonals
* @param {Float64Array} V - input matrix
* @param {integer} strideV1 - stride of the first dimension of `V`
* @param {integer} strideV2 - stride of the second dimension of `V`
* @param {NonNegativeInteger} offsetV - starting index for `V`
* @param {Float64Array} T - input matrix
* @param {integer} strideT1 - stride of the first dimension of `T`
* @param {integer} strideT2 - stride of the second dimension of `T`
* @param {NonNegativeInteger} offsetT - starting index for `T`
* @param {Float64Array} C - input matrix
* @param {integer} strideC1 - stride of the first dimension of `C`
* @param {integer} strideC2 - stride of the second dimension of `C`
* @param {NonNegativeInteger} offsetC - starting index for `C`
* @param {Float64Array} WORK - output matrix
* @param {integer} strideWORK1 - stride of the first dimension of `WORK`
* @param {integer} strideWORK2 - stride of the second dimension of `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
*/
function dlarfb( side, trans, direct, storev, M, N, K, V, strideV1, strideV2, offsetV, T, strideT1, strideT2, offsetT, C, strideC1, strideC2, offsetC, WORK, strideWORK1, strideWORK2, offsetWORK ) { // eslint-disable-line max-len, max-params
	return base( side, trans, direct, storev, M, N, K, V, strideV1, strideV2, offsetV, T, strideT1, strideT2, offsetT, C, strideC1, strideC2, offsetC, WORK, strideWORK1, strideWORK2, offsetWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlarfb;
