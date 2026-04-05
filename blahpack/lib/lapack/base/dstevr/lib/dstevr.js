
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} jobz - jobz
* @param {string} range - range
* @param {NonNegativeInteger} N - N
* @param {Float64Array} d - d
* @param {integer} strideD - strideD
* @param {Float64Array} e - e
* @param {integer} strideE - strideE
* @param {number} vl - vl
* @param {number} vu - vu
* @param {integer} il - il
* @param {integer} iu - iu
* @param {number} abstol - abstol
* @param {Object} out - out
* @param {Float64Array} w - w
* @param {integer} strideW - strideW
* @param {Float64Array} Z - Z
* @param {PositiveInteger} LDZ - leading dimension of `Z`
* @param {Int32Array} ISUPPZ - ISUPPZ
* @param {integer} strideISUPPZ - strideISUPPZ
* @param {Float64Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @param {integer} lwork - lwork
* @param {Int32Array} IWORK - IWORK
* @param {integer} strideIWORK - strideIWORK
* @param {integer} liwork - liwork
* @returns {*} result
*/
function dstevr( jobz, range, N, d, strideD, e, strideE, vl, vu, il, iu, abstol, out, w, strideW, Z, LDZ, ISUPPZ, strideISUPPZ, WORK, strideWORK, lwork, IWORK, strideIWORK, liwork ) { // eslint-disable-line max-len, max-params
	var oisuppz;
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
	oisuppz = stride2offset( N, strideISUPPZ );
	owork = stride2offset( N, strideWORK );
	oiwork = stride2offset( N, strideIWORK );
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Third argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	return base( jobz, range, N, d, strideD, od, e, strideE, oe, vl, vu, il, iu, abstol, out, w, strideW, ow, Z, sz1, sz2, 0, ISUPPZ, strideISUPPZ, oisuppz, WORK, strideWORK, owork, lwork, IWORK, strideIWORK, oiwork, liwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dstevr;
