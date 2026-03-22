

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Generate a real Householder reflector.
*
* @param {NonNegativeInteger} N - number of columns
* @param {number} alpha - scalar constant
* @param {Float64Array} x - input array
* @param {integer} stride - stride length for `x`
* @param {NonNegativeInteger} offset - starting index for `x`
* @param {number} tau - tau
*/
function dlarfg( N, alpha, x, stride, offset, tau ) {
	return base( N, alpha, x, stride, offset, tau );
}


// EXPORTS //

module.exports = dlarfg;
