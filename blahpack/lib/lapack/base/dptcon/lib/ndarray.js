

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Compute the reciprocal of the condition number of a real symmetric positive definite tridiagonal matrix
*
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} d - input array
* @param {integer} strideD - stride length for `d`
* @param {NonNegativeInteger} offsetD - starting index for `d`
* @param {Float64Array} e - input array
* @param {integer} strideE - stride length for `e`
* @param {NonNegativeInteger} offsetE - starting index for `e`
* @param {number} anorm - anorm
* @param {number} rcond - rcond
* @param {Float64Array} WORK - output array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @returns {integer} status code (0 = success)
*/
function dptcon( N, d, strideD, offsetD, e, strideE, offsetE, anorm, rcond, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	return base( N, d, strideD, offsetD, e, strideE, offsetE, anorm, rcond, WORK, strideWORK, offsetWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dptcon;
