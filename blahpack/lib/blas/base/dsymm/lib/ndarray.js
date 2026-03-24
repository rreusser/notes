

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Performs symmetric matrix-matrix multiplication
*
* @param {string} side - specifies the operation type
* @param {string} uplo - specifies the operation type
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {number} alpha - scalar constant
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} B - input matrix
* @param {integer} strideB1 - stride of the first dimension of `B`
* @param {integer} strideB2 - stride of the second dimension of `B`
* @param {NonNegativeInteger} offsetB - starting index for `B`
* @param {number} beta - scalar constant
* @param {Float64Array} C - output matrix
* @param {integer} strideC1 - stride of the first dimension of `C`
* @param {integer} strideC2 - stride of the second dimension of `C`
* @param {NonNegativeInteger} offsetC - starting index for `C`
*/
function dsymm( side, uplo, M, N, alpha, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, beta, C, strideC1, strideC2, offsetC ) { // eslint-disable-line max-len, max-params
	return base( side, uplo, M, N, alpha, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, beta, C, strideC1, strideC2, offsetC ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dsymm;
