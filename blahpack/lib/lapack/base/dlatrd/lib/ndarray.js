

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Reduce NB rows and columns of a symmetric matrix to tridiagonal form
*
* @param {string} uplo - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {integer} nb - nb
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} e - input array
* @param {integer} strideE - stride length for `e`
* @param {NonNegativeInteger} offsetE - starting index for `e`
* @param {Float64Array} TAU - input array
* @param {integer} strideTAU - stride length for `TAU`
* @param {NonNegativeInteger} offsetTAU - starting index for `TAU`
* @param {Float64Array} W - output matrix
* @param {integer} strideW1 - stride of the first dimension of `W`
* @param {integer} strideW2 - stride of the second dimension of `W`
* @param {NonNegativeInteger} offsetW - starting index for `W`
*/
function dlatrd( uplo, N, nb, A, strideA1, strideA2, offsetA, e, strideE, offsetE, TAU, strideTAU, offsetTAU, W, strideW1, strideW2, offsetW ) { // eslint-disable-line max-len, max-params
	return base( uplo, N, nb, A, strideA1, strideA2, offsetA, e, strideE, offsetE, TAU, strideTAU, offsetTAU, W, strideW1, strideW2, offsetW ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlatrd;
