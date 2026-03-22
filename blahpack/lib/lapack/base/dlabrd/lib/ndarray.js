

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Reduce the first NB rows and columns of a matrix to bidiagonal form
*
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {integer} nb - nb
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
* @param {Float64Array} X - input matrix
* @param {integer} strideX1 - stride of the first dimension of `X`
* @param {integer} strideX2 - stride of the second dimension of `X`
* @param {NonNegativeInteger} offsetX - starting index for `X`
* @param {Float64Array} Y - output matrix
* @param {integer} strideY1 - stride of the first dimension of `Y`
* @param {integer} strideY2 - stride of the second dimension of `Y`
* @param {NonNegativeInteger} offsetY - starting index for `Y`
*/
function dlabrd( M, N, nb, A, strideA1, strideA2, offsetA, d, strideD, offsetD, e, strideE, offsetE, TAUQ, strideTAUQ, offsetTAUQ, TAUP, strideTAUP, offsetTAUP, X, strideX1, strideX2, offsetX, Y, strideY1, strideY2, offsetY ) { // eslint-disable-line max-len, max-params
	return base( M, N, nb, A, strideA1, strideA2, offsetA, d, strideD, offsetD, e, strideE, offsetE, TAUQ, strideTAUQ, offsetTAUQ, TAUP, strideTAUP, offsetTAUP, X, strideX1, strideX2, offsetX, Y, strideY1, strideY2, offsetY ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlabrd;
