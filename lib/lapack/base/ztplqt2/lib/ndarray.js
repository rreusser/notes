

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes LQ factorization of a complex triangular-pentagonal matrix using compact WY representation (unblocked)
*
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {integer} l - l
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} B - input matrix
* @param {integer} strideB1 - stride of the first dimension of `B`
* @param {integer} strideB2 - stride of the second dimension of `B`
* @param {NonNegativeInteger} offsetB - starting index for `B`
* @param {Float64Array} T - output matrix
* @param {integer} strideT1 - stride of the first dimension of `T`
* @param {integer} strideT2 - stride of the second dimension of `T`
* @param {NonNegativeInteger} offsetT - starting index for `T`
* @returns {integer} status code (0 = success)
*/
function ztplqt2( M, N, l, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, T, strideT1, strideT2, offsetT ) { // eslint-disable-line max-len, max-params
	return base( M, N, l, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, T, strideT1, strideT2, offsetT ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = ztplqt2;
