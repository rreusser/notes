

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Compute eigenvalues and eigenvectors of a symmetric tridiagonal matrix (complex accumulation)
*
* @param {string} compz - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} d - input array
* @param {integer} strideD - stride length for `d`
* @param {NonNegativeInteger} offsetD - starting index for `d`
* @param {Float64Array} e - input array
* @param {integer} strideE - stride length for `e`
* @param {NonNegativeInteger} offsetE - starting index for `e`
* @param {Float64Array} Z - input matrix
* @param {integer} strideZ1 - stride of the first dimension of `Z`
* @param {integer} strideZ2 - stride of the second dimension of `Z`
* @param {NonNegativeInteger} offsetZ - starting index for `Z`
* @param {Float64Array} WORK - output array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @returns {integer} status code (0 = success)
*/
function zsteqr( compz, N, d, strideD, offsetD, e, strideE, offsetE, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	return base( compz, N, d, strideD, offsetD, e, strideE, offsetE, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zsteqr;
