

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Solves a 1x1 or 2x2 linear system with scaling to prevent overflow
*
* @param {boolean} ltrans - ltrans
* @param {integer} na - na
* @param {integer} nw - nw
* @param {number} smin - smin
* @param {number} ca - ca
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {number} d1 - d1
* @param {number} d2 - d2
* @param {Float64Array} B - input matrix
* @param {integer} strideB1 - stride of the first dimension of `B`
* @param {integer} strideB2 - stride of the second dimension of `B`
* @param {NonNegativeInteger} offsetB - starting index for `B`
* @param {number} wr - wr
* @param {number} wi - wi
* @param {Float64Array} X - output matrix
* @param {integer} strideX1 - stride of the first dimension of `X`
* @param {integer} strideX2 - stride of the second dimension of `X`
* @param {NonNegativeInteger} offsetX - starting index for `X`
* @param {number} scale - scale
* @param {number} xnorm - xnorm
* @returns {integer} status code (0 = success)
*/
function dlaln2( ltrans, na, nw, smin, ca, A, strideA1, strideA2, offsetA, d1, d2, B, strideB1, strideB2, offsetB, wr, wi, X, strideX1, strideX2, offsetX, scale, xnorm ) { // eslint-disable-line max-len, max-params
	return base( ltrans, na, nw, smin, ca, A, strideA1, strideA2, offsetA, d1, d2, B, strideB1, strideB2, offsetB, wr, wi, X, strideX1, strideX2, offsetX, scale, xnorm ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlaln2;
