
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {NonNegativeInteger} N - N
* @param {NonNegativeInteger} nrhs - nrhs
* @param {Float64Array} d - d
* @param {integer} strideD - strideD
* @param {Float64Array} e - e
* @param {integer} strideE - strideE
* @param {Float64Array} DF - DF
* @param {integer} strideDF - strideDF
* @param {Float64Array} EF - EF
* @param {integer} strideEF - strideEF
* @param {Float64Array} B - B
* @param {PositiveInteger} LDB - leading dimension of `B`
* @param {Float64Array} X - X
* @param {PositiveInteger} LDX - leading dimension of `X`
* @param {Float64Array} FERR - FERR
* @param {integer} strideFERR - strideFERR
* @param {Float64Array} BERR - BERR
* @param {integer} strideBERR - strideBERR
* @param {Float64Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @returns {*} result
*/
function dptrfs( N, nrhs, d, strideD, e, strideE, DF, strideDF, EF, strideEF, B, LDB, X, LDX, FERR, strideFERR, BERR, strideBERR, WORK, strideWORK ) { // eslint-disable-line max-len, max-params
	var oberr;
	var oferr;
	var owork;
	var odf;
	var oef;
	var sb1;
	var sb2;
	var sx1;
	var sx2;
	var od;
	var oe;

	sb1 = 1;
	sb2 = LDB;
	sx1 = 1;
	sx2 = LDX;
	od = stride2offset( N, strideD );
	oe = stride2offset( N, strideE );
	odf = stride2offset( N, strideDF );
	oef = stride2offset( N, strideEF );
	oferr = stride2offset( N, strideFERR );
	oberr = stride2offset( N, strideBERR );
	owork = stride2offset( N, strideWORK );
	return base( N, nrhs, d, strideD, od, e, strideE, oe, DF, strideDF, odf, EF, strideEF, oef, B, sb1, sb2, 0, X, sx1, sx2, 0, FERR, strideFERR, oferr, BERR, strideBERR, oberr, WORK, strideWORK, owork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dptrfs;
