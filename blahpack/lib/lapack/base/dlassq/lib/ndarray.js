

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Return an updated sum of squares represented in scaled form
*
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} x - input array
* @param {integer} stride - stride length for `x`
* @param {NonNegativeInteger} offset - starting index for `x`
* @param {number} scale - scale
* @param {number} sumsq - sumsq
*/
function dlassq( N, x, stride, offset, scale, sumsq ) {
	return base( N, x, stride, offset, scale, sumsq );
}


// EXPORTS //

module.exports = dlassq;
