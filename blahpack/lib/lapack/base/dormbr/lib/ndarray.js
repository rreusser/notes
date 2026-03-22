

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Multiply a matrix by the orthogonal matrix from a bidiagonal reduction
*
* @param {string} vect - specifies the operation type
* @param {string} side - specifies the operation type
* @param {string} trans - specifies the operation type
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {NonNegativeInteger} K - number of superdiagonals
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} TAU - input array
* @param {integer} strideTAU - stride length for `TAU`
* @param {NonNegativeInteger} offsetTAU - starting index for `TAU`
* @param {Float64Array} C - input matrix
* @param {integer} strideC1 - stride of the first dimension of `C`
* @param {integer} strideC2 - stride of the second dimension of `C`
* @param {NonNegativeInteger} offsetC - starting index for `C`
* @param {Float64Array} WORK - output array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @param {integer} lwork - lwork
* @returns {integer} status code (0 = success)
*/
function dormbr( vect, side, trans, M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK, lwork ) { // eslint-disable-line max-len, max-params
	return base( vect, side, trans, M, N, K, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK, lwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dormbr;
