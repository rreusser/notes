

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Compute the reciprocal of the condition number of a complex Hermitian positive definite tridiagonal matrix
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
* @param {Float64Array} RWORK - output array
* @param {integer} strideRWORK - stride length for `RWORK`
* @param {NonNegativeInteger} offsetRWORK - starting index for `RWORK`
* @returns {integer} status code (0 = success)
*/
function zptcon( N, d, strideD, offsetD, e, strideE, offsetE, anorm, rcond, RWORK, strideRWORK, offsetRWORK ) { // eslint-disable-line max-len, max-params
	return base( N, d, strideD, offsetD, e, strideE, offsetE, anorm, rcond, RWORK, strideRWORK, offsetRWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zptcon;
