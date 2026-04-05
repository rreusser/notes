
'use strict';

// MODULES //

var isLayout = require( '@stdlib/blas/base/assert/is-layout' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Performs diagonal scaling on a matrix: `X = D * X` where `D` is a diagonal matrix stored as a vector.
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} d - scaling vector of length `M`
* @param {Float64Array} X - input/output matrix
* @param {PositiveInteger} LDX - leading dimension of `X`
* @throws {TypeError} first argument must be a valid order
* @returns {Float64Array} `X`
*/
function dlascl2( order, M, N, d, X, LDX ) {
	var sx1;
	var sx2;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Third argument must be a nonnegative integer. Value: `%d`.', N ) );
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

module.exports = dlascl2;
