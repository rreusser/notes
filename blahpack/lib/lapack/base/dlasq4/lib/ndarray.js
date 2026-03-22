

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Compute approximate singular value for dqds iteration
*
* @param {integer} i0 - i0
* @param {integer} n0 - n0
* @param {Float64Array} z - input array
* @param {integer} stride - stride length for `z`
* @param {NonNegativeInteger} offset - starting index for `z`
* @param {integer} pp - pp
* @param {integer} n0in - n0in
* @param {number} dmin - dmin
* @param {number} dmin1 - dmin1
* @param {number} dmin2 - dmin2
* @param {number} dn - dn
* @param {number} dn1 - dn1
* @param {number} dn2 - dn2
* @param {number} tau - tau
* @param {integer} ttype - ttype
* @param {number} g - g
*/
function dlasq4( i0, n0, z, stride, offset, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g ) { // eslint-disable-line max-len, max-params
	return base( i0, n0, z, stride, offset, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlasq4;
