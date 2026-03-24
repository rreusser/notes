

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Sum of absolute values of a complex vector
*
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} x - input array
* @param {integer} stride - stride length for `x`
* @param {NonNegativeInteger} offset - starting index for `x`
* @param {integer} incx - incx
* @returns {number} result
*/
function dzsum1( N, x, stride, offset, incx ) {
	return base( N, x, stride, offset, incx );
}


// EXPORTS //

module.exports = dzsum1;
