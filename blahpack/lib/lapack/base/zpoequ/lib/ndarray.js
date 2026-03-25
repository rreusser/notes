

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Compute row/column scaling for Hermitian positive definite matrix
*
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} s - output array
* @param {integer} strideS - stride length for `s`
* @param {NonNegativeInteger} offsetS - starting index for `s`
* @param {number} scond - scond
* @param {number} amax - amax
* @returns {integer} status code (0 = success)
*/
function zpoequ( N, A, strideA1, strideA2, offsetA, s, strideS, offsetS, scond, amax ) { // eslint-disable-line max-len, max-params
	return base( N, A, strideA1, strideA2, offsetA, s, strideS, offsetS, scond, amax ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zpoequ;
