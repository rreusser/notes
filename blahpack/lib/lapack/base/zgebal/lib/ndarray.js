

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Balances a general complex matrix for eigenvalue computation
*
* @param {string} job - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {integer} ilo - ilo
* @param {integer} ihi - ihi
* @param {Float64Array} SCALE - output array
* @param {integer} strideSCALE - stride length for `SCALE`
* @param {NonNegativeInteger} offsetSCALE - starting index for `SCALE`
* @returns {integer} status code (0 = success)
*/
function zgebal( job, N, A, strideA1, strideA2, offsetA, ilo, ihi, SCALE, strideSCALE, offsetSCALE ) { // eslint-disable-line max-len, max-params
	return base( job, N, A, strideA1, strideA2, offsetA, ilo, ihi, SCALE, strideSCALE, offsetSCALE ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zgebal;
