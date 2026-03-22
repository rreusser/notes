

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Initialize a matrix to given diagonal and off-diagonal values
*
* @param {string} uplo - specifies the operation type
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {number} alpha - scalar constant
* @param {number} beta - scalar constant
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
*/
function dlaset( uplo, M, N, alpha, beta, A, strideA1, strideA2, offsetA ) { // eslint-disable-line max-len, max-params
	return base( uplo, M, N, alpha, beta, A, strideA1, strideA2, offsetA ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlaset;
