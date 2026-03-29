
'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Rearranges the columns of the M-by-N matrix X as specified by the.
* permutation K(0), K(1), ..., K(N-1) of the integers 0, 1, ..., N-1.
*
* If FORWRD is true, forward permutation:
*   New column I = old column K(I).
*
* If FORWRD is false, backward permutation:
*   New column K(I) = old column I.
*
* ## Notes
*
* -   K is 0-based in base.js. The ndarray.js wrapper converts from 1-based.
* -   The permutation array K is modified during execution (negated entries as
*     markers) but restored to its original state on exit.
*
* @param {boolean} forwrd - if true, apply forward permutation; if false, backward
* @param {NonNegativeInteger} M - number of rows of X
* @param {NonNegativeInteger} N - number of columns of X
* @param {Float64Array} X - input/output matrix (M x N)
* @param {integer} strideX1 - stride of the first dimension of `X`
* @param {integer} strideX2 - stride of the second dimension of `X`
* @param {NonNegativeInteger} offsetX - starting index for `X`
* @param {Int32Array} k - permutation vector (length N), 0-based indices
* @param {integer} strideK - stride length for `k`
* @param {NonNegativeInteger} offsetK - starting index for `k`
*/
function dlapmt( forwrd, M, N, X, strideX1, strideX2, offsetX, k, strideK, offsetK ) { // eslint-disable-line max-len, max-params
	return base( forwrd, M, N, X, strideX1, strideX2, offsetX, k, strideK, offsetK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlapmt;
