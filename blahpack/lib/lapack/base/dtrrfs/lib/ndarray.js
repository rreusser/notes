

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Provides error bounds for solution of a triangular system
*
* @param {string} uplo - specifies the operation type
* @param {string} trans - specifies the operation type
* @param {string} diag - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {integer} nrhs - nrhs
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} B - input matrix
* @param {integer} strideB1 - stride of the first dimension of `B`
* @param {integer} strideB2 - stride of the second dimension of `B`
* @param {NonNegativeInteger} offsetB - starting index for `B`
* @param {Float64Array} X - input matrix
* @param {integer} strideX1 - stride of the first dimension of `X`
* @param {integer} strideX2 - stride of the second dimension of `X`
* @param {NonNegativeInteger} offsetX - starting index for `X`
* @param {Float64Array} FERR - input array
* @param {integer} strideFERR - stride length for `FERR`
* @param {NonNegativeInteger} offsetFERR - starting index for `FERR`
* @param {Float64Array} BERR - input array
* @param {integer} strideBERR - stride length for `BERR`
* @param {NonNegativeInteger} offsetBERR - starting index for `BERR`
* @param {Float64Array} WORK - input array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @param {Int32Array} IWORK - output array
* @param {integer} strideIWORK - stride length for `IWORK`
* @param {NonNegativeInteger} offsetIWORK - starting index for `IWORK`
* @returns {integer} status code (0 = success)
*/
function dtrrfs( uplo, trans, diag, N, nrhs, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, X, strideX1, strideX2, offsetX, FERR, strideFERR, offsetFERR, BERR, strideBERR, offsetBERR, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK ) { // eslint-disable-line max-len, max-params
	return base( uplo, trans, diag, N, nrhs, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, X, strideX1, strideX2, offsetX, FERR, strideFERR, offsetFERR, BERR, strideBERR, offsetBERR, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dtrrfs;
