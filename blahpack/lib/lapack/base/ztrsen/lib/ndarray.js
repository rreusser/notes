

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
 * Reorders the Schur factorization of a complex matrix A = Q*T*Q**H so that a
 * selected cluster of eigenvalues appears in the leading positions on the
 * diagonal of the upper triangular matrix T, and the leading columns of Q form
 * an orthonormal basis of the corresponding right invariant subspace.
 *
 * Optionally computes the reciprocal condition numbers of the cluster of
 * eigenvalues (S) and/or the invariant subspace (SEP).
 *
 *
 * @param {string} job - `'none'`, `'eigenvalues'`, `'subspace'`, or `'both'`
 * @param {string} compq - `'update'` or `'none'`
 * @param {Uint8Array} SELECT - boolean array of length N
 * @param {integer} strideSELECT - stride for SELECT
 * @param {NonNegativeInteger} offsetSELECT - starting index for SELECT
 * @param {NonNegativeInteger} N - order of the matrix T
 * @param {Complex128Array} T - N-by-N upper triangular matrix, modified in place
 * @param {integer} strideT1 - stride of first dimension of T (in complex elements)
 * @param {integer} strideT2 - stride of second dimension of T (in complex elements)
 * @param {NonNegativeInteger} offsetT - starting index for T (in complex elements)
 * @param {Complex128Array} Q - N-by-N unitary matrix, modified if compq='V'
 * @param {integer} strideQ1 - stride of first dimension of Q (in complex elements)
 * @param {integer} strideQ2 - stride of second dimension of Q (in complex elements)
 * @param {NonNegativeInteger} offsetQ - starting index for Q (in complex elements)
 * @param {Complex128Array} W - output: reordered eigenvalues
 * @param {integer} strideW - stride for W (in complex elements)
 * @param {NonNegativeInteger} offsetW - starting index for W (in complex elements)
 * @param {Float64Array} M - output: M[0] = dimension of selected subspace
 * @param {Float64Array} s - output: s[0] = reciprocal condition number for eigenvalues
 * @param {Float64Array} sep - output: sep[0] = reciprocal condition for subspace
 * @param {Complex128Array} WORK - workspace
 * @param {integer} strideWORK - stride for WORK (in complex elements)
 * @param {NonNegativeInteger} offsetWORK - starting index for WORK (in complex elements)
 * @param {integer} lwork - workspace length (in complex elements)
 * @returns {integer} info (0 = success)
 */
function ztrsen( job, compq, SELECT, strideSELECT, offsetSELECT, N, T, strideT1, strideT2, offsetT, Q, strideQ1, strideQ2, offsetQ, w, strideW, offsetW, M, s, sep, WORK, strideWORK, offsetWORK, lwork ) { // eslint-disable-line max-len, max-params
	return base( job, compq, SELECT, strideSELECT, offsetSELECT, N, T, strideT1, strideT2, offsetT, Q, strideQ1, strideQ2, offsetQ, w, strideW, offsetW, M, s, sep, WORK, strideWORK, offsetWORK, lwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = ztrsen;
