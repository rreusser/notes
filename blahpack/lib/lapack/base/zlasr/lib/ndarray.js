

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Apply a sequence of plane rotations to a complex matrix
*
* @param {string} side - specifies the operation type
* @param {string} pivot - specifies the operation type
* @param {string} direct - specifies the operation type
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} c - input array
* @param {integer} strideC - stride length for `c`
* @param {NonNegativeInteger} offsetC - starting index for `c`
* @param {Float64Array} s - input array
* @param {integer} strideS - stride length for `s`
* @param {NonNegativeInteger} offsetS - starting index for `s`
* @param {Float64Array} A - output matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
*/
function zlasr( side, pivot, direct, M, N, c, strideC, offsetC, s, strideS, offsetS, A, strideA1, strideA2, offsetA ) { // eslint-disable-line max-len, max-params
	return base( side, pivot, direct, M, N, c, strideC, offsetC, s, strideS, offsetS, A, strideA1, strideA2, offsetA ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zlasr;
