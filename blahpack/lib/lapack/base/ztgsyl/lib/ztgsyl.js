'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* Solves the generalized Sylvester equation using a level-3 blocked algorithm.
*
* @param {string} trans - trans
* @param {integer} ijob - ijob
* @param {PositiveInteger} M - M
* @param {PositiveInteger} N - N
* @param {Complex128Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Complex128Array} B - B
* @param {PositiveInteger} LDB - leading dimension of `B`
* @param {Complex128Array} C - C
* @param {PositiveInteger} LDC - leading dimension of `C`
* @param {Complex128Array} D - D
* @param {PositiveInteger} LDD - leading dimension of `D`
* @param {Complex128Array} E - E
* @param {PositiveInteger} LDE - leading dimension of `E`
* @param {Complex128Array} F - F
* @param {PositiveInteger} LDF - leading dimension of `F`
* @param {Float64Array} scale - scale
* @param {Float64Array} dif - dif
* @param {Complex128Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @param {integer} lwork - lwork
* @param {Int32Array} IWORK - IWORK
* @param {integer} strideIWORK - strideIWORK
* @returns {*} result
*/
function ztgsyl( trans, ijob, M, N, A, LDA, B, LDB, C, LDC, D, LDD, E, LDE, F, LDF, scale, dif, WORK, strideWORK, lwork, IWORK, strideIWORK ) { // eslint-disable-line max-len, max-params
	var oiwork;
	var owork;
	var sa1;
	var sa2;
	var sb1;
	var sb2;
	var sc1;
	var sc2;
	var sd1;
	var sd2;
	var se1;
	var se2;
	var sf1;
	var sf2;

	sa1 = 1;
	sa2 = LDA;
	sb1 = 1;
	sb2 = LDB;
	sc1 = 1;
	sc2 = LDC;
	sd1 = 1;
	sd2 = LDD;
	se1 = 1;
	se2 = LDE;
	sf1 = 1;
	sf2 = LDF;
	owork = stride2offset( N, strideWORK );
	oiwork = stride2offset( N, strideIWORK );
	return base( trans, ijob, M, N, A, sa1, sa2, 0, B, sb1, sb2, 0, C, sc1, sc2, 0, D, sd1, sd2, 0, E, se1, se2, 0, F, sf1, sf2, 0, scale, dif, WORK, strideWORK, owork, lwork, IWORK, strideIWORK, oiwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = ztgsyl;
