

'use strict';

// MODULES //

var isLayout = require( '@stdlib/blas/base/assert/is-layout' );
var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} order - storage layout (`'row-major'` or `'column-major'`)
* @param {string} uplo - uplo
* @param {string} trans - trans
* @param {string} diag - diag
* @param {NonNegativeInteger} N - N
* @param {NonNegativeInteger} K - K
* @param {Complex128Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Complex128Array} x - x
* @param {integer} strideX - strideX
* @throws {TypeError} first argument must be a valid order
* @returns {*} result
*/
function ztbmv( order, uplo, trans, diag, N, K, A, LDA, x, strideX ) { // eslint-disable-line max-len, max-params
	var sa1;
	var sa2;
	var ox;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}

	if ( order === 'column-major' ) {
		sa1 = 1;
		sa2 = LDA;
	} else {
		sa1 = LDA;
		sa2 = 1;
	}
	ox = stride2offset( N, strideX );
	return base( uplo, trans, diag, N, K, A, sa1, sa2, 0, x, strideX, ox );
}


// EXPORTS //

module.exports = ztbmv;
