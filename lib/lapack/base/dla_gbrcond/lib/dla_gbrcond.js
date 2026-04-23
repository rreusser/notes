
/* eslint-disable max-len, max-params, camelcase */

'use strict';

// MODULES //

var isLayout = require( '@stdlib/blas/base/assert/is-layout' );
var format = require( '@stdlib/string/format' );
var isTransposeOperation = require( '@stdlib/blas/base/assert/is-transpose-operation' );
var max = require( '@stdlib/math/base/special/fast/max' );
var base = require( './base.js' );


// MAIN //

/**
* Estimates the Skeel condition number for a general banded matrix.
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {string} trans - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {integer} kl - kl
* @param {integer} ku - ku
* @param {Float64Array} AB - input matrix
* @param {PositiveInteger} LDAB - leading dimension of `AB`
* @param {Float64Array} AFB - input matrix
* @param {PositiveInteger} LDAFB - leading dimension of `AFB`
* @param {Int32Array} IPIV - input array
* @param {integer} strideIPIV - stride length for `IPIV`
* @param {NonNegativeInteger} offsetIPIV - starting index for `IPIV`
* @param {integer} cmode - cmode
* @param {Float64Array} c - input array
* @param {integer} strideC - stride length for `c`
* @param {Float64Array} WORK - input array
* @param {integer} strideWORK - stride length for `WORK`
* @param {Int32Array} IWORK - output array
* @param {integer} strideIWORK - stride length for `IWORK`
* @param {NonNegativeInteger} offsetIWORK - starting index for `IWORK`
* @throws {TypeError} first argument must be a valid order
* @throws {TypeError} Second argument must be a valid transpose operation
* @throws {RangeError} third argument must be a nonnegative integer
* @throws {RangeError} seventh argument must be sufficiently large
* @throws {RangeError} ninth argument must be sufficiently large
* @returns {number} result
*/
function dla_gbrcond( order, trans, N, kl, ku, AB, LDAB, AFB, LDAFB, IPIV, strideIPIV, offsetIPIV, cmode, c, strideC, WORK, strideWORK, IWORK, strideIWORK, offsetIWORK ) { // eslint-disable-line max-len, max-params
	var safb1;
	var safb2;
	var sab1;
	var sab2;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( !isTransposeOperation( trans ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid transpose operation. Value: `%s`.', trans ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Third argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( order === 'row-major' && LDAB < max( 1, N ) ) {
		throw new RangeError( format( 'invalid argument. Seventh argument must be greater than or equal to max(1,N). Value: `%d`.', LDAB ) );
	}
	if ( order === 'row-major' && LDAFB < max( 1, N ) ) {
		throw new RangeError( format( 'invalid argument. Ninth argument must be greater than or equal to max(1,N). Value: `%d`.', LDAFB ) );
	}
	if ( order === 'column-major' ) {
		sab1 = 1;
		sab2 = LDAB;
		safb1 = 1;
		safb2 = LDAFB;
	} else {
		sab1 = LDAB;
		sab2 = 1;
		safb1 = LDAFB;
		safb2 = 1;
	}
	return base( trans, N, kl, ku, AB, sab1, sab2, 0, AFB, safb1, safb2, 0, IPIV, strideIPIV, offsetIPIV, cmode, c, strideC, 0, WORK, strideWORK, 0, IWORK, strideIWORK, offsetIWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dla_gbrcond;
