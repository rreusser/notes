
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} trans - trans
* @param {NonNegativeInteger} M - M
* @param {NonNegativeInteger} N - N
* @param {NonNegativeInteger} nrhs - nrhs
* @param {Complex128Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Complex128Array} B - B
* @param {PositiveInteger} LDB - leading dimension of `B`
* @param {Complex128Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @param {integer} lwork - lwork
* @returns {*} result
*/
function zgels( trans, M, N, nrhs, A, LDA, B, LDB, WORK, strideWORK, lwork ) { // eslint-disable-line max-len, max-params
	var owork;
	var sa1;
	var sa2;
	var sb1;
	var sb2;

	sa1 = 1;
	sa2 = LDA;
	sb1 = 1;
	sb2 = LDB;
	owork = stride2offset( N, strideWORK );
	return base( trans, M, N, nrhs, A, sa1, sa2, 0, B, sb1, sb2, 0, WORK, strideWORK, owork, lwork );
}


// EXPORTS //

module.exports = zgels;
