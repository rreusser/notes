

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
 * CABS1: |re(z)| + |im(z)|
 *
 *
 * @param {Float64Array} v - Float64 view
 * @param {integer} idx - index of real part
 * @returns {number} CABS1 value
 */
function zgttrf( N, DL, strideDL, offsetDL, d, strideD, offsetD, DU, strideDU, offsetDU, DU2, strideDU2, offsetDU2, IPIV, strideIPIV, offsetIPIV ) { // eslint-disable-line max-len, max-params
	return base( N, DL, strideDL, offsetDL, d, strideD, offsetD, DU, strideDU, offsetDU, DU2, strideDU2, offsetDU2, IPIV, strideIPIV, offsetIPIV ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zgttrf;
