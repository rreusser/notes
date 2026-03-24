

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Solves a tridiagonal system using the LDL^T factorization from dpttrf
*
* @param {NonNegativeInteger} N - number of columns
* @param {integer} nrhs - nrhs
* @param {Float64Array} d - input array
* @param {integer} strideD - stride length for `d`
* @param {NonNegativeInteger} offsetD - starting index for `d`
* @param {Float64Array} e - input array
* @param {integer} strideE - stride length for `e`
* @param {NonNegativeInteger} offsetE - starting index for `e`
* @param {Float64Array} B - output matrix
* @param {integer} strideB1 - stride of the first dimension of `B`
* @param {integer} strideB2 - stride of the second dimension of `B`
* @param {NonNegativeInteger} offsetB - starting index for `B`
*/
function dptts2( N, nrhs, d, strideD, offsetD, e, strideE, offsetE, B, strideB1, strideB2, offsetB ) { // eslint-disable-line max-len, max-params
	return base( N, nrhs, d, strideD, offsetD, e, strideE, offsetE, B, strideB1, strideB2, offsetB ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dptts2;
