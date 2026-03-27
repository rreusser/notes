

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
 * Solves one of the systems of equations A*X = B, A^T*X = B, or A^H*X = B
 * with a complex tridiagonal matrix A using the LU factorization computed by zgttrf.
 *
 * ## Notes
 *
 * -   IPIV values are 0-based (Fortran convention is 1-based).
 * -   `itrans` is an integer: 0 = no transpose, 1 = transpose, 2 = conjugate transpose.
 * -   Complex arrays use complex-element strides/offsets. B strides are in Float64 elements.
 *
 *
 * @param {integer} itrans - 0 for A*X=B, 1 for A^T*X=B, 2 for A^H*X=B
 * @param {NonNegativeInteger} N - order of the matrix A
 * @param {NonNegativeInteger} nrhs - number of right hand sides (columns of B)
 * @param {Complex128Array} DL - multipliers from LU factorization (length N-1)
 * @param {integer} strideDL - stride for `DL` (complex elements)
 * @param {NonNegativeInteger} offsetDL - starting index for `DL` (complex elements)
 * @param {Complex128Array} d - diagonal of U (length N)
 * @param {integer} strideD - stride for `d` (complex elements)
 * @param {NonNegativeInteger} offsetD - starting index for `d` (complex elements)
 * @param {Complex128Array} DU - first superdiagonal of U (length N-1)
 * @param {integer} strideDU - stride for `DU` (complex elements)
 * @param {NonNegativeInteger} offsetDU - starting index for `DU` (complex elements)
 * @param {Complex128Array} DU2 - second superdiagonal of U (length N-2)
 * @param {integer} strideDU2 - stride for `DU2` (complex elements)
 * @param {NonNegativeInteger} offsetDU2 - starting index for `DU2` (complex elements)
 * @param {Int32Array} IPIV - pivot indices (length N), 0-based
 * @param {integer} strideIPIV - stride for `IPIV`
 * @param {NonNegativeInteger} offsetIPIV - starting index for `IPIV`
 * @param {Complex128Array} B - right hand side matrix, overwritten with solution
 * @param {integer} strideB1 - stride of the first dimension of `B` (Float64 elements)
 * @param {integer} strideB2 - stride of the second dimension of `B` (Float64 elements)
 * @param {NonNegativeInteger} offsetB - starting index for `B` (Float64 elements)
 */
function zgtts2( itrans, N, nrhs, DL, strideDL, offsetDL, d, strideD, offsetD, DU, strideDU, offsetDU, DU2, strideDU2, offsetDU2, IPIV, strideIPIV, offsetIPIV, B, strideB1, strideB2, offsetB ) { // eslint-disable-line max-len, max-params
	return base( itrans, N, nrhs, DL, strideDL, offsetDL, d, strideD, offsetD, DU, strideDU, offsetDU, DU2, strideDU2, offsetDU2, IPIV, strideIPIV, offsetIPIV, B, strideB1, strideB2, offsetB ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zgtts2;
