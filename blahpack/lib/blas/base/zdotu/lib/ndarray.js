

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Compute unconjugated dot product of two complex vectors
*
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} x - input array
* @param {integer} strideX - stride length for `x`
* @param {NonNegativeInteger} offsetX - starting index for `x`
* @param {integer} incx - incx
* @param {Float64Array} y - output array
* @param {integer} strideY - stride length for `y`
* @param {NonNegativeInteger} offsetY - starting index for `y`
* @param {integer} incy - incy
* @returns {number} result
*/
function zdotu( N, x, strideX, offsetX, incx, y, strideY, offsetY, incy ) { // eslint-disable-line max-len, max-params
	return base( N, x, strideX, offsetX, incx, y, strideY, offsetY, incy ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zdotu;
