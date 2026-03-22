

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Scale a matrix by CTO/CFROM with overflow protection
*
* @param {string} type - specifies the operation type
* @param {integer} kl - kl
* @param {integer} ku - ku
* @param {number} cfrom - cfrom
* @param {number} cto - cto
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @returns {integer} status code (0 = success)
*/
function dlascl( type, kl, ku, cfrom, cto, M, N, A, strideA1, strideA2, offsetA ) { // eslint-disable-line max-len, max-params
	return base( type, kl, ku, cfrom, cto, M, N, A, strideA1, strideA2, offsetA ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlascl;
