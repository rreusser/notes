

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
* @param {number} anorm - anorm
* @param {Float64Array} rcond - rcond
* @param {Complex128Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @returns {*} result
*/
function zsycon( uplo, N, A, LDA, IPIV, strideIPIV, anorm, rcond, WORK, strideWORK ) { // eslint-disable-line max-len, max-params
	var oipiv;
	var owork;
	var sa1;
	var sa2;

	sa1 = 1;
	sa2 = LDA;
	oipiv = stride2offset( N, strideIPIV );
	owork = stride2offset( N, strideWORK );
	return base( uplo, N, A, sa1, sa2, 0, IPIV, strideIPIV, oipiv, anorm, rcond, WORK, strideWORK, owork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zsycon;
