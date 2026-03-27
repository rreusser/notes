

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
 * CABS1: |re(z)| + |im(z)|
 *
 *
 * @param {Float64Array} v - Float64 view of complex array
 * @param {integer} idx - index of real part
 * @returns {number} CABS1 value
 */
function zgecon( norm, N, A, strideA1, strideA2, offsetA, anorm, rcond, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK ) { // eslint-disable-line max-len, max-params
	return base( norm, N, A, strideA1, strideA2, offsetA, anorm, rcond, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zgecon;
