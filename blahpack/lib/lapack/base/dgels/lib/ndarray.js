

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Solves overdetermined or underdetermined real linear systems involving an
* M-by-N matrix A, or its transpose, using a QR or LQ factorization of A.
*
* @param {string} trans - 'N' for no transpose, 'T' for transpose
* @param {NonNegativeInteger} M - number of rows of A
* @param {NonNegativeInteger} N - number of columns of A
* @param {NonNegativeInteger} nrhs - number of right hand sides (columns of B)
* @param {Float64Array} A - M-by-N matrix, overwritten with factorization on exit
* @param {integer} strideA1 - stride of the first dimension of A
* @param {integer} strideA2 - stride of the second dimension of A
* @param {NonNegativeInteger} offsetA - starting index for A
* @param {Float64Array} B - on entry, RHS matrix; on exit, the solution
* @param {integer} strideB1 - stride of the first dimension of B
* @param {integer} strideB2 - stride of the second dimension of B
* @param {NonNegativeInteger} offsetB - starting index for B
* @returns {integer} info - 0 if successful, >0 if rank-deficient
*/
function dgels( trans, M, N, nrhs, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB ) { // eslint-disable-line max-len, max-params
	return base( trans, M, N, nrhs, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dgels;
