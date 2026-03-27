

'use strict';

// MODULES //

var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
 * Performs the Hermitian packed rank-1 update `A := alpha*x*x**H + A`.
 *
 * `alpha` is a real scalar, `x` is an `N`-element complex vector, and `A` is
 * an `N` by `N` Hermitian matrix supplied in packed form as a Float64Array
 * with interleaved real and imaginary parts.
 *
 *
 * @param {string} uplo - specifies whether upper or lower triangle is stored
 * @param {NonNegativeInteger} N - order of the matrix `A`
 * @param {number} alpha - real scalar constant
 * @param {Float64Array} x - input vector (interleaved re/im pairs)
 * @param {integer} strideX - stride length for `x` (in Float64 elements)
 * @param {NonNegativeInteger} offsetX - starting index for `x` (in Float64 elements)
 * @param {Float64Array} AP - packed Hermitian matrix (interleaved re/im pairs)
 * @param {integer} strideAP - stride length for `AP` (in Float64 elements, typically 2)
 * @param {NonNegativeInteger} offsetAP - starting index for `AP` (in Float64 elements)
 * @throws {TypeError} First argument must be a valid matrix triangle
 * @returns {Float64Array} `AP`
 */
function zhpr( uplo, N, alpha, x, strideX, offsetX, AP, strideAP, offsetAP ) { // eslint-disable-line max-len, max-params
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	return base( uplo, N, alpha, x, strideX, offsetX, AP, strideAP, offsetAP ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zhpr;
