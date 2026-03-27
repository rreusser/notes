

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
 * Computes the dot product of two vectors with extended precision accumulation.
 *
 *
 * @param {PositiveInteger} N - number of indexed elements
 * @param {Float64Array} x - first input array
 * @param {integer} strideX - `x` stride length
 * @param {NonNegativeInteger} offsetX - starting `x` index
 * @param {Float64Array} y - second input array
 * @param {integer} strideY - `y` stride length
 * @param {NonNegativeInteger} offsetY - starting `y` index
 * @returns {number} dot product
 */
function dsdot( N, x, strideX, offsetX, incx, y, strideY, offsetY, incy ) { // eslint-disable-line max-len, max-params
	return base( N, x, strideX, offsetX, incx, y, strideY, offsetY, incy ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dsdot;
