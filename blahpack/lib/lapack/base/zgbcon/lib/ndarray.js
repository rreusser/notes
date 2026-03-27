

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
function zgbcon( norm, N, kl, ku, AB, strideAB1, strideAB2, offsetAB, IPIV, strideIPIV, offsetIPIV, anorm, rcond, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK ) { // eslint-disable-line max-len, max-params
	return base( norm, N, kl, ku, AB, strideAB1, strideAB2, offsetAB, IPIV, strideIPIV, offsetIPIV, anorm, rcond, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zgbcon;
