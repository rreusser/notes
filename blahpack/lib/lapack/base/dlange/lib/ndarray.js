/**
 * Computes the value of the one norm, Frobenius norm, infinity norm, or.
 * largest absolute value of a real matrix.
 *
 *
 * @param {string} norm - `'max'`, `'one-norm'`, `'inf-norm'`, or `'frobenius'`
 * @param {NonNegativeInteger} M - number of rows
 * @param {NonNegativeInteger} N - number of columns
 * @param {Float64Array} A - input matrix
 * @param {integer} strideA1 - stride of the first dimension of `A`
 * @param {integer} strideA2 - stride of the second dimension of `A`
 * @param {NonNegativeInteger} offsetA - starting index for `A`
 * @param {Float64Array} WORK - workspace array (length >= M for 'I' norm)
 * @param {integer} strideWORK - stride length for `WORK`
 * @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
 * @returns {number} norm value
 */

/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Computes the value of the one norm, Frobenius norm, infinity norm, or.
*
* @param {string} norm - `'max'`, `'one-norm'`, `'inf-norm'`, or `'frobenius'`
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} A - input matrix
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} WORK - workspace array (length >= M for 'I' norm)
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @throws {TypeError} first argument must be a valid norm type
* @throws {RangeError} second argument must be a nonnegative integer
* @throws {RangeError} third argument must be a nonnegative integer
* @returns {number} norm value
*/
function dlange( norm, M, N, A, strideA1, strideA2, offsetA, WORK, strideWORK, offsetWORK ) {
	if ( norm !== 'one-norm' && norm !== 'inf-norm' && norm !== 'max' && norm !== 'frobenius' ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid norm type. Value: `%s`.', norm ) );
	}
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Third argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( M === 0 || N === 0 ) {
		return 0.0;
	}
	return base( norm, M, N, A, strideA1, strideA2, offsetA, WORK, strideWORK, offsetWORK );
}


// EXPORTS //

module.exports = dlange;
