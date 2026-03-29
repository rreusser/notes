
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} range - range
* @param {string} order - storage layout (`'row-major'` or `'column-major'`)
* @param {NonNegativeInteger} N - N
* @param {number} vl - vl
* @param {number} vu - vu
* @param {integer} il - il
* @param {integer} iu - iu
* @param {number} abstol - abstol
* @param {Float64Array} d - d
* @param {integer} strideD - strideD
* @param {Float64Array} e - e
* @param {integer} strideE - strideE
* @param {Int32Array} M - M
* @param {Int32Array} nsplit - nsplit
* @param {Float64Array} w - w
* @param {integer} strideW - strideW
* @param {Int32Array} IBLOCK - IBLOCK
* @param {integer} strideIBLOCK - strideIBLOCK
* @param {Int32Array} ISPLIT - ISPLIT
* @param {integer} strideISPLIT - strideISPLIT
* @param {Float64Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @param {Int32Array} IWORK - IWORK
* @param {integer} strideIWORK - strideIWORK
* @returns {*} result
*/
function dstebz( range, order, N, vl, vu, il, iu, abstol, d, strideD, e, strideE, M, nsplit, w, strideW, IBLOCK, strideIBLOCK, ISPLIT, strideISPLIT, WORK, strideWORK, IWORK, strideIWORK ) { // eslint-disable-line max-len, max-params
	var oiblock;
	var oisplit;
	var oiwork;
	var owork;
	var od;
	var oe;
	var ow;

	od = stride2offset( N, strideD );
	oe = stride2offset( N, strideE );
	ow = stride2offset( N, strideW );
	oiblock = stride2offset( N, strideIBLOCK );
	oisplit = stride2offset( N, strideISPLIT );
	owork = stride2offset( N, strideWORK );
	oiwork = stride2offset( N, strideIWORK );
	return base( range, order, N, vl, vu, il, iu, abstol, d, strideD, od, e, strideE, oe, M, nsplit, w, strideW, ow, IBLOCK, strideIBLOCK, oiblock, ISPLIT, strideISPLIT, oisplit, WORK, strideWORK, owork, IWORK, strideIWORK, oiwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dstebz;
