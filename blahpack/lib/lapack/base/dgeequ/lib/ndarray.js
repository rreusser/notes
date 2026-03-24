

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes row and column scalings for equilibrating a general matrix
*
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} r - input array
* @param {integer} strideR - stride length for `r`
* @param {NonNegativeInteger} offsetR - starting index for `r`
* @param {Float64Array} c - output array
* @param {integer} strideC - stride length for `c`
* @param {NonNegativeInteger} offsetC - starting index for `c`
* @param {number} rowcnd - rowcnd
* @param {number} colcnd - colcnd
* @param {number} amax - amax
* @returns {integer} status code (0 = success)
*/
function dgeequ( M, N, A, strideA1, strideA2, offsetA, r, strideR, offsetR, c, strideC, offsetC, rowcnd, colcnd, amax ) { // eslint-disable-line max-len, max-params
	return base( M, N, A, strideA1, strideA2, offsetA, r, strideR, offsetR, c, strideC, offsetC, rowcnd, colcnd, amax ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dgeequ;
