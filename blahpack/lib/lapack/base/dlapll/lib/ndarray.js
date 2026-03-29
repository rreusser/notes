
'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Measures the linear dependence of two vectors X and Y by computing the.
* QR factorization of the N-by-2 matrix (X Y) and returning the smallest
* singular value of the resulting 2-by-2 upper triangular R factor.
*
* On exit, X and Y are overwritten.
*
* @param {NonNegativeInteger} N - length of the vectors
* @param {Float64Array} x - first vector (overwritten)
* @param {integer} strideX - stride for x
* @param {NonNegativeInteger} offsetX - starting index for x
* @param {Float64Array} y - second vector (overwritten)
* @param {integer} strideY - stride for y
* @param {NonNegativeInteger} offsetY - starting index for y
* @param {Float64Array} ssmin - output: ssmin[0] receives the smallest singular value
*/
function dlapll( N, x, strideX, offsetX, y, strideY, offsetY, ssmin ) { // eslint-disable-line max-len, max-params
	return base( N, x, strideX, offsetX, y, strideY, offsetY, ssmin ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlapll;
