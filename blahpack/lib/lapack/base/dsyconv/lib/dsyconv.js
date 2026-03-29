
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} uplo - uplo
* @param {string} way - way
* @param {NonNegativeInteger} N - N
* @param {Float64Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Int32Array} IPIV - IPIV
* @param {integer} strideIPIV - strideIPIV
* @param {Float64Array} E - E
* @param {integer} strideE - strideE
* @returns {*} result
*/
function dsyconv( uplo, way, N, A, LDA, IPIV, strideIPIV, E, strideE ) { // eslint-disable-line max-len, max-params
	var oipiv;
	var sa1;
	var sa2;
	var oe;

	sa1 = 1;
	sa2 = LDA;
	oipiv = stride2offset( N, strideIPIV );
	oe = stride2offset( N, strideE );
	return base( uplo, way, N, A, sa1, sa2, 0, IPIV, strideIPIV, oipiv, E, strideE, oe );
}


// EXPORTS //

module.exports = dsyconv;
