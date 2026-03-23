

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Solve a complex triangular system of equations with a single right-hand side
*
* @param {string} uplo - specifies the operation type
* @param {string} trans - specifies the operation type
* @param {string} diag - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} x - output array
* @param {integer} strideX - stride length for `x`
* @param {NonNegativeInteger} offsetX - starting index for `x`
*/
function ztrsv( uplo, trans, diag, N, A, strideA1, strideA2, offsetA, x, strideX, offsetX ) { // eslint-disable-line max-len, max-params
	return base( uplo, trans, diag, N, A, strideA1, strideA2, offsetA, x, strideX, offsetX ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = ztrsv;
