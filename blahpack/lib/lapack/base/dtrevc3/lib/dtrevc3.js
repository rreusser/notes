

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} side - side
* @param {string} howmny - howmny
* @param {(Uint8Array|Array)} SELECT - SELECT
* @param {integer} strideSELECT - strideSELECT
* @param {NonNegativeInteger} N - N
* @param {Float64Array} T - T
* @param {PositiveInteger} LDT - leading dimension of `T`
* @param {Float64Array} VL - VL
* @param {PositiveInteger} LDVL - leading dimension of `VL`
* @param {Float64Array} VR - VR
* @param {PositiveInteger} LDVR - leading dimension of `VR`
* @param {integer} mm - mm
* @param {integer} M - M
* @param {Float64Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @param {integer} lwork - lwork
* @returns {*} result
*/
function dtrevc3( side, howmny, SELECT, strideSELECT, N, T, LDT, VL, LDVL, VR, LDVR, mm, M, WORK, strideWORK, lwork ) { // eslint-disable-line max-len, max-params
	var oselect;
	var owork;
	var svl1;
	var svl2;
	var svr1;
	var svr2;
	var st1;
	var st2;

	st1 = 1;
	st2 = LDT;
	svl1 = 1;
	svl2 = LDVL;
	svr1 = 1;
	svr2 = LDVR;
	oselect = stride2offset( N, strideSELECT );
	owork = stride2offset( N, strideWORK );
	return base( side, howmny, SELECT, strideSELECT, oselect, N, T, st1, st2, 0, VL, svl1, svl2, 0, VR, svr1, svr2, 0, mm, M, WORK, strideWORK, owork, lwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dtrevc3;
