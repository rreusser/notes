

'use strict';

// MODULES //

var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
 * Performs the Hermitian packed matrix-vector operation `y := alpha*A*x + beta*y`.
 *
 * `A` is an `N` by `N` Hermitian matrix supplied in packed form, `x` and `y`
 * are `N`-element complex vectors, and `alpha` and `beta` are complex scalars.
 *
 *
 * @param {string} uplo - specifies whether upper or lower triangle is stored
 * @param {NonNegativeInteger} N - order of the matrix `A`
 * @param {Complex128} alpha - complex scalar multiplier for `A*x`
 * @param {Complex128Array} AP - packed Hermitian matrix
 * @param {integer} strideAP - stride length for `AP` (in complex elements)
 * @param {NonNegativeInteger} offsetAP - starting index for `AP` (in complex elements)
 * @param {Complex128Array} x - complex input vector
 * @param {integer} strideX - stride length for `x` (in complex elements)
 * @param {NonNegativeInteger} offsetX - starting index for `x` (in complex elements)
 * @param {Complex128} beta - complex scalar multiplier for `y`
 * @param {Complex128Array} y - complex input/output vector
 * @param {integer} strideY - stride length for `y` (in complex elements)
 * @param {NonNegativeInteger} offsetY - starting index for `y` (in complex elements)
 * @throws {TypeError} First argument must be a valid matrix triangle
 * @returns {Complex128Array} `y`
 */
function zhpmv( uplo, N, alpha, AP, strideAP, offsetAP, x, strideX, offsetX, beta, y, strideY, offsetY ) { // eslint-disable-line max-len, max-params
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	return base( uplo, N, alpha, AP, strideAP, offsetAP, x, strideX, offsetX, beta, y, strideY, offsetY ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zhpmv;
