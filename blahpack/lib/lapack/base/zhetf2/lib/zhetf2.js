

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
* @param {Complex128Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Int32Array} IPIV - IPIV
* @param {integer} strideIPIV - strideIPIV
* @returns {*} result
*/
function zhetf2( uplo, N, A, LDA, IPIV, strideIPIV ) {
	var oipiv;
	var sa1;
	var sa2;

	sa1 = 1;
	sa2 = LDA;
	oipiv = stride2offset( N, strideIPIV );
	return base( uplo, N, A, sa1, sa2, 0, IPIV, strideIPIV, oipiv );
}


// EXPORTS //

module.exports = zhetf2;
