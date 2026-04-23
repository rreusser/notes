/**
 * Multiplies a real M-by-N matrix A by the real scalar CTO/CFROM, doing.
 * the multiplication safely with respect to overflow and underflow via
 * iterative scaling.
 *
 * TYPE specifies which matrix elements are accessed:
 * 'G' - general full matrix
 * 'L' - lower triangular
 * 'U' - upper triangular
 * 'H' - upper Hessenberg
 * 'B' - lower half of symmetric band (kl+1 rows, N cols)
 * 'Q' - upper half of symmetric band (ku+1 rows, N cols)
 * 'Z' - band matrix (2*kl+ku+1 rows, N cols)
 *
 *
 * @param {string} type - `'general'`, `'lower'`, `'upper'`, `'upper-hessenberg'`, `'lower-band'`, `'upper-band'`, or `'band'`
 * @param {integer} kl - lower bandwidth (for banded types)
 * @param {integer} ku - upper bandwidth (for banded types)
 * @param {number} cfrom - scale denominator (must be nonzero)
 * @param {number} cto - scale numerator
 * @param {NonNegativeInteger} M - rows
 * @param {NonNegativeInteger} N - columns
 * @param {Float64Array} A - matrix
 * @param {integer} strideA1 - first dimension stride
 * @param {integer} strideA2 - second dimension stride
 * @param {NonNegativeInteger} offsetA - starting index for A
 * @returns {integer} 0 on success
 */

/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Multiplies a real M-by-N matrix A by the real scalar CTO/CFROM, doing.
*
* @param {string} type - `'general'`, `'lower'`, `'upper'`, `'upper-hessenberg'`, `'lower-band'`, `'upper-band'`, or `'band'`
* @param {integer} kl - lower bandwidth (for banded types)
* @param {integer} ku - upper bandwidth (for banded types)
* @param {number} cfrom - scale denominator (must be nonzero)
* @param {number} cto - scale numerator
* @param {NonNegativeInteger} M - rows
* @param {NonNegativeInteger} N - columns
* @param {Float64Array} A - matrix
* @param {integer} strideA1 - first dimension stride
* @param {integer} strideA2 - second dimension stride
* @param {NonNegativeInteger} offsetA - starting index for A
* @throws {TypeError} first argument must be a valid matrix type
* @throws {RangeError} sixth argument must be a nonnegative integer
* @throws {RangeError} seventh argument must be a nonnegative integer
* @returns {integer} 0 on success
*/
function dlascl( type, kl, ku, cfrom, cto, M, N, A, strideA1, strideA2, offsetA ) {
	if ( type !== 'general' && type !== 'lower' && type !== 'upper' && type !== 'hessenberg' && type !== 'band-lower' && type !== 'band-upper' ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid matrix type. Value: `%s`.', type ) );
	}
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. Sixth argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Seventh argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( M === 0 || N === 0 ) {
		return 0;
	}
	return base( type, kl, ku, cfrom, cto, M, N, A, strideA1, strideA2, offsetA );
}


// EXPORTS //

module.exports = dlascl;
