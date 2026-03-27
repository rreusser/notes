

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} fact - fact
* @param {string} uplo - uplo
* @param {NonNegativeInteger} N - N
* @param {NonNegativeInteger} nrhs - nrhs
* @param {Complex128Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Complex128Array} AF - AF
* @param {PositiveInteger} LDAF - leading dimension of `AF`
* @param {string} equed - equed
* @param {Float64Array} s - s
* @param {integer} strideS - strideS
* @param {Complex128Array} B - B
* @param {PositiveInteger} LDB - leading dimension of `B`
* @param {Complex128Array} X - X
* @param {PositiveInteger} LDX - leading dimension of `X`
* @param {Float64Array} rcond - rcond
* @param {Float64Array} FERR - FERR
* @param {integer} strideFERR - strideFERR
* @param {Float64Array} BERR - BERR
* @param {integer} strideBERR - strideBERR
* @param {Complex128Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @param {Float64Array} RWORK - RWORK
* @param {integer} strideRWORK - strideRWORK
* @returns {*} result
*/
function zposvx( fact, uplo, N, nrhs, A, LDA, AF, LDAF, equed, s, strideS, B, LDB, X, LDX, rcond, FERR, strideFERR, BERR, strideBERR, WORK, strideWORK, RWORK, strideRWORK ) { // eslint-disable-line max-len, max-params
	var orwork;
	var oberr;
	var oferr;
	var owork;
	var saf1;
	var saf2;
	var sa1;
	var sa2;
	var sb1;
	var sb2;
	var sx1;
	var sx2;
	var os;

	sa1 = 1;
	sa2 = LDA;
	saf1 = 1;
	saf2 = LDAF;
	sb1 = 1;
	sb2 = LDB;
	sx1 = 1;
	sx2 = LDX;
	os = stride2offset( N, strideS );
	oferr = stride2offset( N, strideFERR );
	oberr = stride2offset( N, strideBERR );
	owork = stride2offset( N, strideWORK );
	orwork = stride2offset( N, strideRWORK );
	return base( fact, uplo, N, nrhs, A, sa1, sa2, 0, AF, saf1, saf2, 0, equed, s, strideS, os, B, sb1, sb2, 0, X, sx1, sx2, 0, rcond, FERR, strideFERR, oferr, BERR, strideBERR, oberr, WORK, strideWORK, owork, RWORK, strideRWORK, orwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zposvx;
