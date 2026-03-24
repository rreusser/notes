

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Solve a complex general tridiagonal system of linear equations A * X = B
*
* @param {NonNegativeInteger} N - number of columns
* @param {integer} nrhs - nrhs
* @param {Float64Array} DL - input array
* @param {integer} strideDL - stride length for `DL`
* @param {NonNegativeInteger} offsetDL - starting index for `DL`
* @param {Float64Array} d - input array
* @param {integer} strideD - stride length for `d`
* @param {NonNegativeInteger} offsetD - starting index for `d`
* @param {Float64Array} DU - input array
* @param {integer} strideDU - stride length for `DU`
* @param {NonNegativeInteger} offsetDU - starting index for `DU`
* @param {Float64Array} B - output matrix
* @param {integer} strideB1 - stride of the first dimension of `B`
* @param {integer} strideB2 - stride of the second dimension of `B`
* @param {NonNegativeInteger} offsetB - starting index for `B`
* @returns {integer} status code (0 = success)
*/
function zgtsv( N, nrhs, DL, strideDL, offsetDL, d, strideD, offsetD, DU, strideDU, offsetDU, B, strideB1, strideB2, offsetB ) { // eslint-disable-line max-len, max-params
	return base( N, nrhs, DL, strideDL, offsetDL, d, strideD, offsetD, DU, strideDU, offsetDU, B, strideB1, strideB2, offsetB ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zgtsv;
