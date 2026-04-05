
'use strict';

// MODULES //

var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Computes the sum of the absolute values of the real and imaginary components of a complex vector.
*
* `dzasum` takes the sum of `(|Re(.)| + |Im(.)|)` for each element and returns a double-precision result.
*
* @param {NonNegativeInteger} N - number of indexed elements
* @param {Complex128Array} zx - complex input vector
* @param {integer} strideX - stride in complex elements
* @param {NonNegativeInteger} offsetX - starting index (in complex elements)
* @returns {number} sum of absolute values
*/
function dzasum( N, x, stride, offset, incx ) {
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	return base( N, x, stride, offset, incx );
}


// EXPORTS //

module.exports = dzasum;
