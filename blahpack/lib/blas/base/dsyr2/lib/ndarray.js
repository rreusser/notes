

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Perform symmetric rank-2 update
*
* @param {string} uplo - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {number} alpha - scalar constant
* @param {Float64Array} x - input array
* @param {integer} strideX - stride length for `x`
* @param {NonNegativeInteger} offsetX - starting index for `x`
* @param {Float64Array} y - input array
* @param {integer} strideY - stride length for `y`
* @param {NonNegativeInteger} offsetY - starting index for `y`
* @param {Float64Array} A - output matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
*/
function dsyr2( uplo, N, alpha, x, strideX, offsetX, y, strideY, offsetY, A, strideA1, strideA2, offsetA ) { // eslint-disable-line max-len, max-params
	return base( uplo, N, alpha, x, strideX, offsetX, y, strideY, offsetY, A, strideA1, strideA2, offsetA ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dsyr2;
