

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes a QR factorization with column pivoting using Level 2 BLAS
*
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {integer} offset - offset
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Int32Array} JPVT - input array
* @param {integer} strideJPVT - stride length for `JPVT`
* @param {NonNegativeInteger} offsetJPVT - starting index for `JPVT`
* @param {Float64Array} TAU - input array
* @param {integer} strideTAU - stride length for `TAU`
* @param {NonNegativeInteger} offsetTAU - starting index for `TAU`
* @param {Float64Array} VN1 - input array
* @param {integer} strideVN1 - stride length for `VN1`
* @param {NonNegativeInteger} offsetVN1 - starting index for `VN1`
* @param {Float64Array} VN2 - input array
* @param {integer} strideVN2 - stride length for `VN2`
* @param {NonNegativeInteger} offsetVN2 - starting index for `VN2`
* @param {Float64Array} WORK - output array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
*/
function dlaqp2( M, N, offset, A, strideA1, strideA2, offsetA, JPVT, strideJPVT, offsetJPVT, TAU, strideTAU, offsetTAU, VN1, strideVN1, offsetVN1, VN2, strideVN2, offsetVN2, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	return base( M, N, offset, A, strideA1, strideA2, offsetA, JPVT, strideJPVT, offsetJPVT, TAU, strideTAU, offsetTAU, VN1, strideVN1, offsetVN1, VN2, strideVN2, offsetVN2, WORK, strideWORK, offsetWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlaqp2;
