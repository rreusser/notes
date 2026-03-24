

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Solves a system of linear equations with an LU factored matrix using complete pivoting
*
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} RHS - input array
* @param {integer} strideRHS - stride length for `RHS`
* @param {NonNegativeInteger} offsetRHS - starting index for `RHS`
* @param {Int32Array} IPIV - input array
* @param {integer} strideIPIV - stride length for `IPIV`
* @param {NonNegativeInteger} offsetIPIV - starting index for `IPIV`
* @param {Int32Array} JPIV - output array
* @param {integer} strideJPIV - stride length for `JPIV`
* @param {NonNegativeInteger} offsetJPIV - starting index for `JPIV`
* @param {number} scale - scale
*/
function dgesc2( N, A, strideA1, strideA2, offsetA, RHS, strideRHS, offsetRHS, IPIV, strideIPIV, offsetIPIV, JPIV, strideJPIV, offsetJPIV, scale ) { // eslint-disable-line max-len, max-params
	return base( N, A, strideA1, strideA2, offsetA, RHS, strideRHS, offsetRHS, IPIV, strideIPIV, offsetIPIV, JPIV, strideJPIV, offsetJPIV, scale ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dgesc2;
