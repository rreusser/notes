

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} uplo - uplo
* @param {NonNegativeInteger} N - N
* @param {Complex128} alpha - alpha
* @param {Complex128Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Complex128Array} x - x
* @param {integer} strideX - strideX
* @param {Complex128} beta - beta
* @param {Complex128Array} y - y
* @param {integer} strideY - strideY
* @returns {*} result
*/
function zsymv( uplo, N, alpha, A, LDA, x, strideX, beta, y, strideY ) { // eslint-disable-line max-len, max-params
	var sa1;
	var sa2;
	var ox;
	var oy;

	sa1 = 1;
	sa2 = LDA;
	ox = stride2offset( N, strideX );
	oy = stride2offset( N, strideY );
	return base( uplo, N, alpha, A, sa1, sa2, 0, x, strideX, ox, beta, y, strideY, oy );
}


// EXPORTS //

module.exports = zsymv;
