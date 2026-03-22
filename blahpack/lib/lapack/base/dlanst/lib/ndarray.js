

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Compute the norm of a symmetric tridiagonal matrix
*
* @param {string} norm - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} d - input array
* @param {integer} strideD - stride length for `d`
* @param {NonNegativeInteger} offsetD - starting index for `d`
* @param {Float64Array} e - output array
* @param {integer} strideE - stride length for `e`
* @param {NonNegativeInteger} offsetE - starting index for `e`
* @returns {number} result
*/
function dlanst( norm, N, d, strideD, offsetD, e, strideE, offsetE ) { // eslint-disable-line max-len, max-params
	return base( norm, N, d, strideD, offsetD, e, strideE, offsetE ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlanst;
