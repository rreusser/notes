
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
* @param {Float64Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} B - B
* @param {PositiveInteger} LDB - leading dimension of `B`
* @param {Float64Array} w - w
* @param {integer} strideW - strideW
* @param {Float64Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @param {integer} lwork - lwork
* @returns {*} result
*/
function dsygv( itype, jobz, uplo, N, A, LDA, B, LDB, w, strideW, WORK, strideWORK, lwork ) { // eslint-disable-line max-len, max-params
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
	return base( itype, jobz, uplo, N, A, sa1, sa2, 0, B, sb1, sb2, 0, w, strideW, ow, WORK, strideWORK, owork, lwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dsygv;
