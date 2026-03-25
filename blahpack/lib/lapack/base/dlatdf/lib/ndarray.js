

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes contribution to reciprocal DIF estimate using LU factorization from dgetc2
*
* @param {integer} ijob - ijob
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} Z - input matrix
* @param {integer} strideZ1 - stride of the first dimension of `Z`
* @param {integer} strideZ2 - stride of the second dimension of `Z`
* @param {NonNegativeInteger} offsetZ - starting index for `Z`
* @param {Float64Array} RHS - input array
* @param {integer} strideRHS - stride length for `RHS`
* @param {NonNegativeInteger} offsetRHS - starting index for `RHS`
* @param {number} rdsum - rdsum
* @param {number} rdscal - rdscal
* @param {Int32Array} IPIV - input array
* @param {integer} strideIPIV - stride length for `IPIV`
* @param {NonNegativeInteger} offsetIPIV - starting index for `IPIV`
* @param {Int32Array} JPIV - output array
* @param {integer} strideJPIV - stride length for `JPIV`
* @param {NonNegativeInteger} offsetJPIV - starting index for `JPIV`
*/
function dlatdf( ijob, N, Z, strideZ1, strideZ2, offsetZ, RHS, strideRHS, offsetRHS, rdsum, rdscal, IPIV, strideIPIV, offsetIPIV, JPIV, strideJPIV, offsetJPIV ) { // eslint-disable-line max-len, max-params
	return base( ijob, N, Z, strideZ1, strideZ2, offsetZ, RHS, strideRHS, offsetRHS, rdsum, rdscal, IPIV, strideIPIV, offsetIPIV, JPIV, strideJPIV, offsetJPIV ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlatdf;
