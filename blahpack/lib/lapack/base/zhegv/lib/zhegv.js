

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {integer} itype - itype
* @param {string} jobz - jobz
* @param {string} uplo - uplo
* @param {NonNegativeInteger} N - N
* @param {Complex128Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Complex128Array} B - B
* @param {PositiveInteger} LDB - leading dimension of `B`
* @param {Float64Array} w - w
* @param {integer} strideW - strideW
* @param {Complex128Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @param {integer} lwork - lwork
* @param {Float64Array} RWORK - RWORK
* @param {integer} strideRWORK - strideRWORK
* @returns {*} result
*/
function zhegv( itype, jobz, uplo, N, A, LDA, B, LDB, w, strideW, WORK, strideWORK, lwork, RWORK, strideRWORK ) { // eslint-disable-line max-len, max-params
	var orwork;
	var owork;
	var sa1;
	var sa2;
	var sb1;
	var sb2;
	var ow;

	sa1 = 1;
	sa2 = LDA;
	sb1 = 1;
	sb2 = LDB;
	ow = stride2offset( N, strideW );
	owork = stride2offset( N, strideWORK );
	orwork = stride2offset( N, strideRWORK );
	return base( itype, jobz, uplo, N, A, sa1, sa2, 0, B, sb1, sb2, 0, w, strideW, ow, WORK, strideWORK, owork, lwork, RWORK, strideRWORK, orwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zhegv;
