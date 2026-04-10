

/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var isLayout = require( '@stdlib/blas/base/assert/is-layout' );
var format = require( '@stdlib/string/format' );
var max = require( '@stdlib/math/base/special/fast/max' );
var base = require( './base.js' );


// MAIN //

/**
* Solves a real quasi-triangular system of equations
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {boolean} ltran - ltran
* @param {boolean} lreal - lreal
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} T - input matrix
* @param {PositiveInteger} LDT - leading dimension of `T`
* @param {Float64Array} b - input array
* @param {integer} strideB - stride length for `b`
* @param {number} w - w
* @param {number} scale - scale
* @param {Float64Array} x - input array
* @param {integer} strideX - stride length for `x`
* @param {Float64Array} WORK - output array
* @param {integer} strideWORK - stride length for `WORK`
* @throws {TypeError} first argument must be a valid order
* @returns {integer} status code (0 = success)
*/
function dlaqtr( order, ltran, lreal, N, T, LDT, b, strideB, w, scale, x, strideX, WORK, strideWORK ) { // eslint-disable-line max-len, max-params
	var st1;
	var st2;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( order === 'row-major' && LDT < max( 1, N ) ) {
		throw new RangeError( format( 'invalid argument. Sixth argument must be greater than or equal to max(1,N). Value: `%d`.', LDT ) );
	}
	if ( order === 'column-major' ) {
		st1 = 1;
		st2 = LDT;
	} else {
		st1 = LDT;
		st2 = 1;
	}
	return base( ltran, lreal, N, T, st1, st2, 0, b, strideB, 0, w, scale, x, strideX, 0, WORK, strideWORK, 0 ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlaqtr;
