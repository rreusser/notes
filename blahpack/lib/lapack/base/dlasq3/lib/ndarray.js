

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Check for deflation and compute shift for dqds iteration
*
* @param {integer} i0 - i0
* @param {integer} n0 - n0
* @param {Float64Array} z - input array
* @param {integer} stride - stride length for `z`
* @param {NonNegativeInteger} offset - starting index for `z`
* @param {integer} pp - pp
* @param {number} dmin - dmin
* @param {number} sigma - sigma
* @param {number} desig - desig
* @param {number} qmax - qmax
* @param {integer} nfail - nfail
* @param {integer} iter - iter
* @param {integer} ndiv - ndiv
* @param {boolean} ieee - ieee
* @param {integer} ttype - ttype
* @param {number} dmin1 - dmin1
* @param {number} dmin2 - dmin2
* @param {number} dn - dn
* @param {number} dn1 - dn1
* @param {number} dn2 - dn2
* @param {number} g - g
* @param {number} tau - tau
*/
function dlasq3( i0, n0, z, stride, offset, pp, dmin, sigma, desig, qmax, nfail, iter, ndiv, ieee, ttype, dmin1, dmin2, dn, dn1, dn2, g, tau ) { // eslint-disable-line max-len, max-params
	return base( i0, n0, z, stride, offset, pp, dmin, sigma, desig, qmax, nfail, iter, ndiv, ieee, ttype, dmin1, dmin2, dn, dn1, dn2, g, tau ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlasq3;
