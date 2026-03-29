
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {NonNegativeInteger} M - M
* @param {NonNegativeInteger} N - N
* @param {NonNegativeInteger} nrhs - nrhs
* @param {Complex128Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Complex128Array} B - B
* @param {PositiveInteger} LDB - leading dimension of `B`
* @param {Float64Array} S - S
* @param {integer} strideS - strideS
* @param {number} rcond - rcond
* @param {Array} rank - rank
* @param {Complex128Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @param {integer} lwork - lwork
* @param {Float64Array} RWORK - RWORK
* @param {integer} strideRWORK - strideRWORK
* @returns {*} result
*/
function zgelss( M, N, nrhs, A, LDA, B, LDB, S, strideS, rcond, rank, WORK, strideWORK, lwork, RWORK, strideRWORK ) { // eslint-disable-line max-len, max-params
	var orwork;
	var owork;
	var sa1;
	var sa2;
	var sb1;
	var sb2;
	var os;

	sa1 = 1;
	sa2 = LDA;
	sb1 = 1;
	sb2 = LDB;
	os = stride2offset( N, strideS );
	owork = stride2offset( N, strideWORK );
	orwork = stride2offset( N, strideRWORK );
	return base( M, N, nrhs, A, sa1, sa2, 0, B, sb1, sb2, 0, S, strideS, os, rcond, rank, WORK, strideWORK, owork, lwork, RWORK, strideRWORK, orwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zgelss;
