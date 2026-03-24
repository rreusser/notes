

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Estimate 1-norm of a square matrix using reverse communication
*
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} v - input array
* @param {integer} strideV - stride length for `v`
* @param {NonNegativeInteger} offsetV - starting index for `v`
* @param {Float64Array} x - output array
* @param {integer} strideX - stride length for `x`
* @param {NonNegativeInteger} offsetX - starting index for `x`
* @param {number} est - est
* @param {integer} kase - kase
* @param {Int32Array} isave - input array
* @param {integer} strideISAVE - stride length for `isave`
* @param {NonNegativeInteger} offsetISAVE - starting index for `isave`
*/
function zlacn2( N, v, strideV, offsetV, x, strideX, offsetX, est, kase, isave, strideISAVE, offsetISAVE ) { // eslint-disable-line max-len, max-params
	return base( N, v, strideV, offsetV, x, strideX, offsetX, est, kase, isave, strideISAVE, offsetISAVE ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zlacn2;
