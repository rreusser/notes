

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Find index of first element of maximum absolute value
*
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} x - input array
* @param {integer} stride - stride length for `x`
* @param {NonNegativeInteger} offset - starting index for `x`
* @param {integer} incx - incx
* @returns {number} result
*/
function izmax1( N, x, stride, offset, incx ) {
	return base( N, x, stride, offset, incx );
}


// EXPORTS //

module.exports = izmax1;
