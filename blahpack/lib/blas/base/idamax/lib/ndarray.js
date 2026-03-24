

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Find the index of element with maximum absolute value
*
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} x - input array
* @param {integer} stride - stride length for `x`
* @param {NonNegativeInteger} offset - starting index for `x`
* @returns {number} result
*/
function idamax( N, x, stride, offset ) {
	return base( N, x, stride, offset );
}


// EXPORTS //

module.exports = idamax;
