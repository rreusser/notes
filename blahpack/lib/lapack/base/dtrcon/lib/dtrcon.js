
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} norm - norm
* @param {string} uplo - uplo
* @param {string} diag - diag
* @param {NonNegativeInteger} N - N
* @param {Float64Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} RCOND - RCOND
* @param {Float64Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @param {Int32Array} IWORK - IWORK
* @param {integer} strideIWORK - strideIWORK
* @returns {*} result
*/
function dtrcon( norm, uplo, diag, N, A, LDA, RCOND, WORK, strideWORK, IWORK, strideIWORK ) { // eslint-disable-line max-len, max-params
	var oiwork;
	var owork;
	var sa1;
	var sa2;

	sa1 = 1;
	sa2 = LDA;
	owork = stride2offset( N, strideWORK );
	oiwork = stride2offset( N, strideIWORK );
	return base( norm, uplo, diag, N, A, sa1, sa2, 0, RCOND, WORK, strideWORK, owork, IWORK, strideIWORK, oiwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dtrcon;
