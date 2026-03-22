

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes one dqds transform in ping-pong form with a shift.
*
* @param {integer} i0 - first index (1-based)
* @param {integer} n0 - last index (1-based)
* @param {Float64Array} z - input/output array
* @param {integer} stride - stride length for `z`
* @param {NonNegativeInteger} offset - starting index for `z`
* @param {integer} pp - ping-pong flag (0 or 1)
* @param {number} tau - shift value
* @param {number} sigma - accumulated shift
* @param {boolean} ieee - flag for IEEE arithmetic
* @param {number} eps - machine epsilon
* @returns {Object} object with properties: dmin, dmin1, dmin2, dn, dnm1, dnm2
*/
function dlasq5( i0, n0, z, stride, offset, pp, tau, sigma, ieee, eps ) { // eslint-disable-line max-len, max-params
	return base( i0, n0, z, stride, offset, pp, tau, sigma, ieee, eps );
}


// EXPORTS //

module.exports = dlasq5;
