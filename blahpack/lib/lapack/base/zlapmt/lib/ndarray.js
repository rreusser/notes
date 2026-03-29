

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Rearrange columns of a complex matrix as specified by a permutation vector
*
* @param {boolean} forwrd - forwrd
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Complex128Array} X - input/output matrix (M x N)
* @param {integer} strideX1 - stride of the first dimension of `X`
* @param {integer} strideX2 - stride of the second dimension of `X`
* @param {NonNegativeInteger} offsetX - starting index for `X`
* @param {Int32Array} k - permutation vector (length N), 0-based indices
* @param {integer} strideK - stride length for `k`
* @param {NonNegativeInteger} offsetK - starting index for `k`
*/
function zlapmt( forwrd, M, N, X, strideX1, strideX2, offsetX, k, strideK, offsetK ) { // eslint-disable-line max-len, max-params
	return base( forwrd, M, N, X, strideX1, strideX2, offsetX, k, strideK, offsetK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zlapmt;
