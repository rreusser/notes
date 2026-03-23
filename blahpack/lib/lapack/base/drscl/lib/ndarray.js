

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Scale a vector by the reciprocal of a scalar
*
* @param {NonNegativeInteger} N - number of columns
* @param {number} sa - sa
* @param {Float64Array} x - input array
* @param {integer} stride - stride length for `x`
* @param {NonNegativeInteger} offset - starting index for `x`
* @param {integer} incx - incx
*/
function drscl( N, sa, x, stride, offset, incx ) {
	return base( N, sa, x, stride, offset, incx );
}


// EXPORTS //

module.exports = drscl;
