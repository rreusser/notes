
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Rearranges the rows of an M-by-N matrix X as specified by a permutation vector.
*
* @param {boolean} forwrd - if true, apply forward permutation; if false, backward
* @param {NonNegativeInteger} M - number of rows of X
* @param {NonNegativeInteger} N - number of columns of X
* @param {Float64Array} X - input/output matrix (M x N)
* @param {PositiveInteger} LDX - leading dimension of `X`
* @param {Int32Array} k - permutation vector (length M)
* @param {integer} strideK - stride length for `k`
*/
function dlapmr( forwrd, M, N, X, LDX, k, strideK ) { // eslint-disable-line max-len, max-params
	var sx1;
	var sx2;
	var ok;

	sx1 = 1;
	sx2 = LDX;
	ok = stride2offset( M, strideK );
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Third argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	base( forwrd, M, N, X, sx1, sx2, 0, k, strideK, ok );
}


// EXPORTS //

module.exports = dlapmr;
