/**
 * Copies a vector x to a vector y.
 *
 *
 * @param {PositiveInteger} N - number of indexed elements
 * @param {Float64Array} x - input array
 * @param {integer} strideX - `x` stride length
 * @param {NonNegativeInteger} offsetX - starting `x` index
 * @param {Float64Array} y - output array
 * @param {integer} strideY - `y` stride length
 * @param {NonNegativeInteger} offsetY - starting `y` index
 * @returns {Float64Array} `y`
 */

/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Copies a vector x to a vector y.
*
* @param {PositiveInteger} N - number of indexed elements
* @param {Float64Array} x - input array
* @param {integer} strideX - `x` stride length
* @param {NonNegativeInteger} offsetX - starting `x` index
* @param {Float64Array} y - output array
* @param {integer} strideY - `y` stride length
* @param {NonNegativeInteger} offsetY - starting `y` index
* @throws {RangeError} first argument must be a nonnegative integer
* @returns {Float64Array} `y`
*/
function dcopy( N, x, strideX, offsetX, y, strideY, offsetY ) {
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( N === 0 ) {
		return y;
	}
	return base( N, x, strideX, offsetX, y, strideY, offsetY );
}


// EXPORTS //

module.exports = dcopy;
