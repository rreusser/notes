
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
* @param {Float64Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} WR - WR
* @param {integer} strideWR - strideWR
* @param {Float64Array} WI - WI
* @param {integer} strideWI - strideWI
* @param {Float64Array} VL - VL
* @param {PositiveInteger} LDVL - leading dimension of `VL`
* @param {Float64Array} VR - VR
* @param {PositiveInteger} LDVR - leading dimension of `VR`
* @returns {*} result
*/
function dgeev( jobvl, jobvr, N, A, LDA, WR, strideWR, WI, strideWI, VL, LDVL, VR, LDVR ) { // eslint-disable-line max-len, max-params
	var svl1;
	var svl2;
	var svr1;
	var svr2;
	var owi;
	var owr;
	var sa1;
	var sa2;

	sa1 = 1;
	sa2 = LDA;
	svl1 = 1;
	svl2 = LDVL;
	svr1 = 1;
	svr2 = LDVR;
	owr = stride2offset( N, strideWR );
	owi = stride2offset( N, strideWI );
	return base( jobvl, jobvr, N, A, sa1, sa2, 0, WR, strideWR, owr, WI, strideWI, owi, VL, svl1, svl2, 0, VR, svr1, svr2, 0 ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dgeev;
