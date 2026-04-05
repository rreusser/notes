
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {NonNegativeInteger} N - N
* @param {Float64Array} d - d
* @param {integer} strideD - strideD
* @param {Float64Array} e - e
* @param {integer} strideE - strideE
* @param {NonNegativeInteger} M - M
* @param {Float64Array} w - w
* @param {integer} strideW - strideW
* @param {Int32Array} IBLOCK - IBLOCK
* @param {integer} strideIBLOCK - strideIBLOCK
* @param {Int32Array} ISPLIT - ISPLIT
* @param {integer} strideISPLIT - strideISPLIT
* @param {Float64Array} Z - Z
* @param {PositiveInteger} LDZ - leading dimension of `Z`
* @param {Float64Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @param {Int32Array} IWORK - IWORK
* @param {integer} strideIWORK - strideIWORK
* @param {Int32Array} IFAIL - IFAIL
* @param {integer} strideIFAIL - strideIFAIL
* @returns {*} result
*/
function dstein( N, d, strideD, e, strideE, M, w, strideW, IBLOCK, strideIBLOCK, ISPLIT, strideISPLIT, Z, LDZ, WORK, strideWORK, IWORK, strideIWORK, IFAIL, strideIFAIL ) { // eslint-disable-line max-len, max-params
	var oiblock;
	var oisplit;
	var oifail;
	var oiwork;
	var owork;
	var sz1;
	var sz2;
	var od;
	var oe;
	var ow;

	sz1 = 1;
	sz2 = LDZ;
	od = stride2offset( N, strideD );
	oe = stride2offset( N, strideE );
	ow = stride2offset( N, strideW );
	oiblock = stride2offset( N, strideIBLOCK );
	oisplit = stride2offset( N, strideISPLIT );
	owork = stride2offset( N, strideWORK );
	oiwork = stride2offset( N, strideIWORK );
	oifail = stride2offset( N, strideIFAIL );
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. Sixth argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	return base( N, d, strideD, od, e, strideE, oe, M, w, strideW, ow, IBLOCK, strideIBLOCK, oiblock, ISPLIT, strideISPLIT, oisplit, Z, sz1, sz2, 0, WORK, strideWORK, owork, IWORK, strideIWORK, oiwork, IFAIL, strideIFAIL, oifail ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dstein;
