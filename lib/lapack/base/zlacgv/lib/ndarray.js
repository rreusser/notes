/**
 * Conjugate a complex vector in-place.
 *
 *
 * @param {NonNegativeInteger} N - number of complex elements
 * @param {Complex128Array} x - complex input vector
 * @param {integer} stride - stride for `x` (in complex elements)
 * @param {NonNegativeInteger} offset - starting index for `x` (in complex elements)
 * @returns {Complex128Array} `x`
 */

/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Conjugate a complex vector in-place.
*
* @param {NonNegativeInteger} N - number of complex elements
* @param {Complex128Array} x - complex input vector
* @param {integer} stride - stride for `x` (in complex elements)
* @param {NonNegativeInteger} offset - starting index for `x` (in complex elements)
* @throws {RangeError} first argument must be a nonnegative integer
* @returns {Complex128Array} `x`
*/
function zlacgv( N, x, stride, offset ) {
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( N === 0 ) {
		return x;
	}
	return base( N, x, stride, offset );
}


// EXPORTS //

module.exports = zlacgv;
