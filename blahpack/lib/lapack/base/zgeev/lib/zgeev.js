

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} jobvl - jobvl
* @param {string} jobvr - jobvr
* @param {NonNegativeInteger} N - N
* @param {Complex128Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Complex128Array} w - w
* @param {integer} strideW - strideW
* @param {Complex128Array} VL - VL
* @param {PositiveInteger} LDVL - leading dimension of `VL`
* @param {Complex128Array} VR - VR
* @param {PositiveInteger} LDVR - leading dimension of `VR`
* @param {Complex128Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @param {integer} lwork - lwork
* @param {Float64Array} RWORK - RWORK
* @param {integer} strideRWORK - strideRWORK
* @returns {*} result
*/
function zgeev( jobvl, jobvr, N, A, LDA, w, strideW, VL, LDVL, VR, LDVR, WORK, strideWORK, lwork, RWORK, strideRWORK ) { // eslint-disable-line max-len, max-params
	var orwork;
	var owork;
	var svl1;
	var svl2;
	var svr1;
	var svr2;
	var sa1;
	var sa2;
	var ow;

	sa1 = 1;
	sa2 = LDA;
	svl1 = 1;
	svl2 = LDVL;
	svr1 = 1;
	svr2 = LDVR;
	ow = stride2offset( N, strideW );
	owork = stride2offset( N, strideWORK );
	orwork = stride2offset( N, strideRWORK );
	return base( jobvl, jobvr, N, A, sa1, sa2, 0, w, strideW, ow, VL, svl1, svl2, 0, VR, svr1, svr2, 0, WORK, strideWORK, owork, lwork, RWORK, strideRWORK, orwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zgeev;
