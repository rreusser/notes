

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Perform one of the matrix-vector operations y := alpha*A*x + beta*y or y := alpha*A**T*x + beta*y or y := alpha*A**H*x + beta*y where A is a general band matrix.
*
* @param {string} trans - specifies the operation type
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {integer} kl - kl
* @param {integer} ku - ku
* @param {Complex128} alpha - scalar constant
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} x - input array
* @param {integer} strideX - stride length for `x`
* @param {NonNegativeInteger} offsetX - starting index for `x`
* @param {Complex128} beta - scalar constant
* @param {Float64Array} y - output array
* @param {integer} strideY - stride length for `y`
* @param {NonNegativeInteger} offsetY - starting index for `y`
*/
function zgbmv( trans, M, N, kl, ku, alpha, A, strideA1, strideA2, offsetA, x, strideX, offsetX, beta, y, strideY, offsetY ) { // eslint-disable-line max-len, max-params
	return base( trans, M, N, kl, ku, alpha, A, strideA1, strideA2, offsetA, x, strideX, offsetX, beta, y, strideY, offsetY ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zgbmv;
