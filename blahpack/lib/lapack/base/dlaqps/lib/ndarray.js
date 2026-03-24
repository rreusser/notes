

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes a step of QR factorization with column pivoting using Level 3 BLAS
*
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {integer} offset - offset
* @param {integer} nb - nb
* @param {integer} kb - kb
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
* @param {Float64Array} AUXV - input array
* @param {integer} strideAUXV - stride length for `AUXV`
* @param {NonNegativeInteger} offsetAUXV - starting index for `AUXV`
* @param {Float64Array} F - output matrix
* @param {integer} strideF1 - stride of the first dimension of `F`
* @param {integer} strideF2 - stride of the second dimension of `F`
* @param {NonNegativeInteger} offsetF - starting index for `F`
*/
function dlaqps( M, N, offset, nb, kb, A, strideA1, strideA2, offsetA, JPVT, strideJPVT, offsetJPVT, TAU, strideTAU, offsetTAU, VN1, strideVN1, offsetVN1, VN2, strideVN2, offsetVN2, AUXV, strideAUXV, offsetAUXV, F, strideF1, strideF2, offsetF ) { // eslint-disable-line max-len, max-params
	return base( M, N, offset, nb, kb, A, strideA1, strideA2, offsetA, JPVT, strideJPVT, offsetJPVT, TAU, strideTAU, offsetTAU, VN1, strideVN1, offsetVN1, VN2, strideVN2, offsetVN2, AUXV, strideAUXV, offsetAUXV, F, strideF1, strideF2, offsetF ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlaqps;
