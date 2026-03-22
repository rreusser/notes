

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Sort an array of doubles in increasing or decreasing order
*
* @param {string} id - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} d - input array
* @param {integer} stride - stride length for `d`
* @param {NonNegativeInteger} offset - starting index for `d`
* @returns {integer} status code (0 = success)
*/
function dlasrt( id, N, d, stride, offset ) {
	return base( id, N, d, stride, offset );
}


// EXPORTS //

module.exports = dlasrt;
