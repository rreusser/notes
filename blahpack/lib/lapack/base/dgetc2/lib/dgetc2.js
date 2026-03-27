

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {NonNegativeInteger} N - N
* @param {Float64Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Int32Array} IPIV - IPIV
* @param {integer} strideIPIV - strideIPIV
* @param {Int32Array} JPIV - JPIV
* @param {integer} strideJPIV - strideJPIV
* @returns {*} result
*/
function dgetc2( N, A, LDA, IPIV, strideIPIV, JPIV, strideJPIV ) { // eslint-disable-line max-len, max-params
	var oipiv;
	var ojpiv;
	var sa1;
	var sa2;

	sa1 = 1;
	sa2 = LDA;
	oipiv = stride2offset( N, strideIPIV );
	ojpiv = stride2offset( N, strideJPIV );
	return base( N, A, sa1, sa2, 0, IPIV, strideIPIV, oipiv, JPIV, strideJPIV, ojpiv );
}


// EXPORTS //

module.exports = dgetc2;
