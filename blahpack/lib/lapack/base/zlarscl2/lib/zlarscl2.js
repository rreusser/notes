
'use strict';

// MODULES //

var isLayout = require( '@stdlib/blas/base/assert/is-layout' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Performs reciprocal diagonal scaling on a complex matrix: `X = D^{-1} * X` where `D` is a real diagonal matrix stored as a vector.
*
* @param {string} order - storage layout (`row-major` or `column-major`)
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} d - real diagonal scaling vector of length `M`
* @param {Complex128Array} X - input/output complex matrix
* @param {PositiveInteger} LDX - leading dimension of `X`
* @throws {TypeError} first argument must be a valid order
* @returns {Complex128Array} `X`
*/
function zlarscl2( order, M, N, d, X, LDX ) {
	var sx1;
	var sx2;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( order === 'column-major' ) {
		sx1 = 1;
		sx2 = LDX;
	} else {
		sx1 = LDX;
		sx2 = 1;
	}
	return base( M, N, d, 1, 0, X, sx1, sx2, 0 );
}


// EXPORTS //

module.exports = zlarscl2;
