

'use strict';

// MODULES //

var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
 * Reorders the Schur factorization of a complex matrix A = Q*T*Q^H, so that
 * the diagonal element of T with row index IFST is moved to row ILST.
 *
 * The Schur form T is reordered by a unitary similarity transformation
 * Z^H * T * Z, and optionally the matrix Q of Schur vectors is updated by
 * postmultiplication with Z.
 *
 * In the complex case, T is upper triangular (no 2x2 blocks), so this is
 * simpler than the real case (dtrexc).
 *
 * Note: IFST and ILST are 1-based (Fortran convention).
 *
 *
 * @param {string} compq - `'update'` to update Q, `'none'` to not update Q
 * @param {NonNegativeInteger} N - order of the matrix T
 * @param {Complex128Array} T - the upper triangular Schur matrix
 * @param {integer} strideT1 - stride of the first dimension of T (complex elements)
 * @param {integer} strideT2 - stride of the second dimension of T (complex elements)
 * @param {NonNegativeInteger} offsetT - starting index for T (complex elements)
 * @param {Complex128Array} Q - unitary matrix (updated if compq=`'update'`)
 * @param {integer} strideQ1 - stride of the first dimension of Q (complex elements)
 * @param {integer} strideQ2 - stride of the second dimension of Q (complex elements)
 * @param {NonNegativeInteger} offsetQ - starting index for Q (complex elements)
 * @param {integer} ifst - row index of the element to move (1-based)
 * @param {integer} ilst - target row index (1-based)
 * @throws {TypeError} First argument must be a valid compq value
 * @returns {integer} info - 0 on success
 */
function ztrexc( compq, N, T, strideT1, strideT2, offsetT, Q, strideQ1, strideQ2, offsetQ, ifst, ilst ) { // eslint-disable-line max-len, max-params
	if ( compq !== 'none' && compq !== 'initialize' && compq !== 'update' ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid compq value. Value: `%s`.', compq ) );
	}
	return base( compq, N, T, strideT1, strideT2, offsetT, Q, strideQ1, strideQ2, offsetQ, ifst, ilst ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = ztrexc;
