
'use strict';

// MODULES //

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
	return base( N, x, stride, offset, incx );
}


// EXPORTS //

module.exports = dzasum;
