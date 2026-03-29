
'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Finds the index of the first element having the maximum absolute value.
*
* @param {NonNegativeInteger} N - number of indexed elements
* @param {Float64Array} x - input array
* @param {integer} strideX - stride length for `x`
* @param {NonNegativeInteger} offsetX - starting index for `x`
* @returns {integer} 0-based index of the max element, or -1 if N < 1
*/
function idamax( N, x, stride, offset ) {
	return base( N, x, stride, offset );
}


// EXPORTS //

module.exports = idamax;
