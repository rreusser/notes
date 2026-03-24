

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Multiply a general tridiagonal matrix by a rectangular matrix
*
* @param {string} trans - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {integer} nrhs - nrhs
* @param {number} alpha - scalar constant
* @param {Float64Array} DL - input array
* @param {integer} strideDL - stride length for `DL`
* @param {NonNegativeInteger} offsetDL - starting index for `DL`
* @param {Float64Array} d - input array
* @param {integer} strideD - stride length for `d`
* @param {NonNegativeInteger} offsetD - starting index for `d`
* @param {Float64Array} DU - input array
* @param {integer} strideDU - stride length for `DU`
* @param {NonNegativeInteger} offsetDU - starting index for `DU`
* @param {Float64Array} X - input matrix
* @param {integer} strideX1 - stride of the first dimension of `X`
* @param {integer} strideX2 - stride of the second dimension of `X`
* @param {NonNegativeInteger} offsetX - starting index for `X`
* @param {number} beta - scalar constant
* @param {Float64Array} B - output matrix
* @param {integer} strideB1 - stride of the first dimension of `B`
* @param {integer} strideB2 - stride of the second dimension of `B`
* @param {NonNegativeInteger} offsetB - starting index for `B`
*/
function dlagtm( trans, N, nrhs, alpha, DL, strideDL, offsetDL, d, strideD, offsetD, DU, strideDU, offsetDU, X, strideX1, strideX2, offsetX, beta, B, strideB1, strideB2, offsetB ) { // eslint-disable-line max-len, max-params
	return base( trans, N, nrhs, alpha, DL, strideDL, offsetDL, d, strideD, offsetD, DU, strideDU, offsetDU, X, strideX1, strideX2, offsetX, beta, B, strideB1, strideB2, offsetB ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlagtm;
