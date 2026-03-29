
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
* @param {NonNegativeInteger} nrhs - nrhs
* @param {Float64Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Int32Array} IPIV - IPIV
* @param {integer} strideIPIV - strideIPIV
* @param {Float64Array} B - B
* @param {PositiveInteger} LDB - leading dimension of `B`
* @returns {*} result
*/
function dsysv( uplo, N, nrhs, A, LDA, IPIV, strideIPIV, B, LDB ) { // eslint-disable-line max-len, max-params
	var oipiv;
	var sa1;
	var sa2;
	var sb1;
	var sb2;

	sa1 = 1;
	sa2 = LDA;
	sb1 = 1;
	sb2 = LDB;
	oipiv = stride2offset( N, strideIPIV );
	return base( uplo, N, nrhs, A, sa1, sa2, 0, IPIV, strideIPIV, oipiv, B, sb1, sb2, 0 );
}


// EXPORTS //

module.exports = dsysv;
