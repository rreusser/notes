

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Find the last non-zero row of a real matrix.
*
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @returns {number} result
*/
function iladlr( M, N, A, strideA1, strideA2, offsetA ) {
	return base( M, N, A, strideA1, strideA2, offsetA );
}


// EXPORTS //

module.exports = iladlr;
