

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Solves the real Sylvester matrix equation for 1-by-1 or 2-by-2 matrices
*
* @param {boolean} ltranl - ltranl
* @param {boolean} ltranr - ltranr
* @param {integer} isgn - isgn
* @param {integer} n1 - n1
* @param {integer} n2 - n2
* @param {Float64Array} TL - input matrix
* @param {integer} strideTL1 - stride of the first dimension of `TL`
* @param {integer} strideTL2 - stride of the second dimension of `TL`
* @param {NonNegativeInteger} offsetTL - starting index for `TL`
* @param {Float64Array} TR - input matrix
* @param {integer} strideTR1 - stride of the first dimension of `TR`
* @param {integer} strideTR2 - stride of the second dimension of `TR`
* @param {NonNegativeInteger} offsetTR - starting index for `TR`
* @param {Float64Array} B - input matrix
* @param {integer} strideB1 - stride of the first dimension of `B`
* @param {integer} strideB2 - stride of the second dimension of `B`
* @param {NonNegativeInteger} offsetB - starting index for `B`
* @param {number} scale - scale
* @param {Float64Array} X - output matrix
* @param {integer} strideX1 - stride of the first dimension of `X`
* @param {integer} strideX2 - stride of the second dimension of `X`
* @param {NonNegativeInteger} offsetX - starting index for `X`
* @param {number} xnorm - xnorm
* @returns {integer} status code (0 = success)
*/
function dlasy2( ltranl, ltranr, isgn, n1, n2, TL, strideTL1, strideTL2, offsetTL, TR, strideTR1, strideTR2, offsetTR, B, strideB1, strideB2, offsetB, scale, X, strideX1, strideX2, offsetX, xnorm ) { // eslint-disable-line max-len, max-params
	return base( ltranl, ltranr, isgn, n1, n2, TL, strideTL1, strideTL2, offsetTL, TR, strideTR1, strideTR2, offsetTR, B, strideB1, strideB2, offsetB, scale, X, strideX1, strideX2, offsetX, xnorm ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlasy2;
