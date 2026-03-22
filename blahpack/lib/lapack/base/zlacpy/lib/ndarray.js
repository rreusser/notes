

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Copy all or part of a complex matrix
*
* @param {string} uplo - specifies the operation type
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} B - output matrix
* @param {integer} strideB1 - stride of the first dimension of `B`
* @param {integer} strideB2 - stride of the second dimension of `B`
* @param {NonNegativeInteger} offsetB - starting index for `B`
*/
function zlacpy( uplo, M, N, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB ) { // eslint-disable-line max-len, max-params
	return base( uplo, M, N, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zlacpy;
