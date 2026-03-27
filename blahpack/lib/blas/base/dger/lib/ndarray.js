/**
 * Performs the rank 1 operation A := alpha_x_y**T + A.
 *
 *
 * @param {NonNegativeInteger} M - number of rows of A
 * @param {NonNegativeInteger} N - number of columns of A
 * @param {number} alpha - scalar multiplier
 * @param {Float64Array} x - first input vector
 * @param {integer} strideX - stride for x
 * @param {NonNegativeInteger} offsetX - starting index for x
 * @param {Float64Array} y - second input vector
 * @param {integer} strideY - stride for y
 * @param {NonNegativeInteger} offsetY - starting index for y
 * @param {Float64Array} A - input/output matrix
 * @param {integer} strideA1 - stride of first dimension of A
 * @param {integer} strideA2 - stride of second dimension of A
 * @param {NonNegativeInteger} offsetA - starting index for A
 * @returns {Float64Array} A
 */

/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Performs the rank 1 operation A := alpha.
*
* @param {NonNegativeInteger} M - number of rows of A
* @param {NonNegativeInteger} N - number of columns of A
* @param {number} alpha - scalar multiplier
* @param {Float64Array} x - first input vector
* @param {integer} strideX - stride for x
* @param {NonNegativeInteger} offsetX - starting index for x
* @param {Float64Array} y - second input vector
* @param {integer} strideY - stride for y
* @param {NonNegativeInteger} offsetY - starting index for y
* @param {Float64Array} A - input/output matrix
* @param {integer} strideA1 - stride of first dimension of A
* @param {integer} strideA2 - stride of second dimension of A
* @param {NonNegativeInteger} offsetA - starting index for A
* @throws {RangeError} first argument must be a nonnegative integer
* @throws {RangeError} second argument must be a nonnegative integer
* @returns {Float64Array} A
*/
function dger( M, N, alpha, x, strideX, offsetX, y, strideY, offsetY, A, strideA1, strideA2, offsetA ) {
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( M === 0 || N === 0 ) {
		return y;
	}
	return base( M, N, alpha, x, strideX, offsetX, y, strideY, offsetY, A, strideA1, strideA2, offsetA );
}


// EXPORTS //

module.exports = dger;
