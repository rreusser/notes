

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Apply a Givens plane rotation
*
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} x - input array
* @param {integer} strideX - stride length for `x`
* @param {NonNegativeInteger} offsetX - starting index for `x`
* @param {Float64Array} y - output array
* @param {integer} strideY - stride length for `y`
* @param {NonNegativeInteger} offsetY - starting index for `y`
* @param {number} c - c
* @param {number} s - s
*/
function drot( N, x, strideX, offsetX, y, strideY, offsetY, c, s ) { // eslint-disable-line max-len, max-params
	return base( N, x, strideX, offsetX, y, strideY, offsetY, c, s ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = drot;
