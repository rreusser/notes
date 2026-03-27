/**
 * Perform the rank 1 operation A := alpha_x_y**T + A (unconjugated),.
 * where alpha is a complex scalar, x is an M element complex vector,
 * y is an N element complex vector, and A is an M by N complex matrix.
 *
 * Unlike zgerc, this routine does NOT conjugate y.
 *
 *
 * @param {NonNegativeInteger} M - number of rows
 * @param {NonNegativeInteger} N - number of columns
 * @param {Complex128} alpha - complex scalar
 * @param {Complex128Array} x - first complex input vector
 * @param {integer} strideX - stride for `x` (in complex elements)
 * @param {NonNegativeInteger} offsetX - starting index for `x` (in complex elements)
 * @param {Complex128Array} y - second complex input vector
 * @param {integer} strideY - stride for `y` (in complex elements)
 * @param {NonNegativeInteger} offsetY - starting index for `y` (in complex elements)
 * @param {Complex128Array} A - complex matrix
 * @param {integer} strideA1 - stride of the first dimension of `A` (in complex elements)
 * @param {integer} strideA2 - stride of the second dimension of `A` (in complex elements)
 * @param {NonNegativeInteger} offsetA - starting index for `A` (in complex elements)
 * @returns {Complex128Array} `A`
 */

/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Perform the rank 1 operation A := alpha.
*
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Complex128} alpha - complex scalar
* @param {Complex128Array} x - first complex input vector
* @param {integer} strideX - stride for `x` (in complex elements)
* @param {NonNegativeInteger} offsetX - starting index for `x` (in complex elements)
* @param {Complex128Array} y - second complex input vector
* @param {integer} strideY - stride for `y` (in complex elements)
* @param {NonNegativeInteger} offsetY - starting index for `y` (in complex elements)
* @param {Complex128Array} A - complex matrix
* @param {integer} strideA1 - stride of the first dimension of `A` (in complex elements)
* @param {integer} strideA2 - stride of the second dimension of `A` (in complex elements)
* @param {NonNegativeInteger} offsetA - starting index for `A` (in complex elements)
* @throws {RangeError} first argument must be a nonnegative integer
* @throws {RangeError} second argument must be a nonnegative integer
* @returns {Complex128Array} `A`
*/
function zgeru( M, N, alpha, x, strideX, offsetX, y, strideY, offsetY, A, strideA1, strideA2, offsetA ) {
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( M === 0 || N === 0 ) {
		return A;
	}
	return base( M, N, alpha, x, strideX, offsetX, y, strideY, offsetY, A, strideA1, strideA2, offsetA );
}


// EXPORTS //

module.exports = zgeru;
