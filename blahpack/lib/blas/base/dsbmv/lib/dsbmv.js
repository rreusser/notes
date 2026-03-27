

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
* @param {NonNegativeInteger} N - N
* @param {NonNegativeInteger} K - K
* @param {number} alpha - alpha
* @param {Float64Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} x - x
* @param {integer} strideX - strideX
* @param {number} beta - beta
* @param {Float64Array} y - y
* @param {integer} strideY - strideY
* @throws {TypeError} first argument must be a valid order
* @returns {*} result
*/
function dsbmv( order, uplo, N, K, alpha, A, LDA, x, strideX, beta, y, strideY ) { // eslint-disable-line max-len, max-params
	var sa1;
	var sa2;
	var ox;
	var oy;

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
	oy = stride2offset( N, strideY );
	return base( uplo, N, K, alpha, A, sa1, sa2, 0, x, strideX, ox, beta, y, strideY, oy );
}


// EXPORTS //

module.exports = dsbmv;
