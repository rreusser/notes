
'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes an RQ factorization of a complex M-by-N matrix A = R * Q.
* using blocked Householder reflections.
*
* On exit, if M <= N, the upper triangle of the subarray
* A(0:M-1, N-M:N-1) contains the M-by-M upper triangular matrix R;
* if M >= N, the elements on and above the (M-N)-th subdiagonal
* contain the M-by-N upper trapezoidal matrix R; the remaining
* elements, with the array TAU, represent the unitary matrix Q.
*
* @param {NonNegativeInteger} M - number of rows in A
* @param {NonNegativeInteger} N - number of columns in A
* @param {Complex128Array} A - input/output matrix (column-major)
* @param {integer} strideA1 - stride of the first dimension of A (in complex elements)
* @param {integer} strideA2 - stride of the second dimension of A (in complex elements)
* @param {NonNegativeInteger} offsetA - starting index for A (in complex elements)
* @param {Complex128Array} TAU - output array of scalar factors (length min(M,N))
* @param {integer} strideTAU - stride for TAU (in complex elements)
* @param {NonNegativeInteger} offsetTAU - starting index for TAU (in complex elements)
* @param {Complex128Array} WORK - workspace array
* @param {integer} strideWORK - stride for WORK (in complex elements)
* @param {NonNegativeInteger} offsetWORK - starting index for WORK (in complex elements)
* @param {integer} lwork - dimension of WORK array (ignored; workspace allocated internally if needed)
* @returns {integer} info - 0 if successful
*/
function zgerqf( M, N, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK, lwork ) { // eslint-disable-line max-len, max-params
	return base( M, N, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK, lwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zgerqf;
