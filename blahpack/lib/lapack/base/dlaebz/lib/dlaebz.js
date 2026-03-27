

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {integer} ijob - ijob
* @param {integer} nitmax - nitmax
* @param {NonNegativeInteger} N - N
* @param {integer} mmax - mmax
* @param {integer} minp - minp
* @param {integer} nbmin - nbmin
* @param {number} abstol - abstol
* @param {number} reltol - reltol
* @param {number} pivmin - pivmin
* @param {Float64Array} d - d
* @param {integer} strideD - strideD
* @param {Float64Array} e - e
* @param {integer} strideE - strideE
* @param {Float64Array} E2 - E2
* @param {integer} strideE2 - strideE2
* @param {Int32Array} NVAL - NVAL
* @param {integer} strideNVAL - strideNVAL
* @param {Float64Array} AB - AB
* @param {PositiveInteger} LDAB - leading dimension of `AB`
* @param {Float64Array} c - c
* @param {integer} strideC - strideC
* @param {Int32Array} mout - mout
* @param {Int32Array} NAB - NAB
* @param {PositiveInteger} LDNAB - leading dimension of `NAB`
* @param {Float64Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @param {Int32Array} IWORK - IWORK
* @param {integer} strideIWORK - strideIWORK
* @returns {*} result
*/
function dlaebz( ijob, nitmax, N, mmax, minp, nbmin, abstol, reltol, pivmin, d, strideD, e, strideE, E2, strideE2, NVAL, strideNVAL, AB, LDAB, c, strideC, mout, NAB, LDNAB, WORK, strideWORK, IWORK, strideIWORK ) { // eslint-disable-line max-len, max-params
	var oiwork;
	var onval;
	var owork;
	var snab1;
	var snab2;
	var sab1;
	var sab2;
	var oe2;
	var oc;
	var od;
	var oe;

	sab1 = 1;
	sab2 = LDAB;
	snab1 = 1;
	snab2 = LDNAB;
	od = stride2offset( N, strideD );
	oe = stride2offset( N, strideE );
	oe2 = stride2offset( N, strideE2 );
	onval = stride2offset( N, strideNVAL );
	oc = stride2offset( N, strideC );
	owork = stride2offset( N, strideWORK );
	oiwork = stride2offset( N, strideIWORK );
	return base( ijob, nitmax, N, mmax, minp, nbmin, abstol, reltol, pivmin, d, strideD, od, e, strideE, oe, E2, strideE2, oe2, NVAL, strideNVAL, onval, AB, sab1, sab2, 0, c, strideC, oc, mout, NAB, snab1, snab2, 0, WORK, strideWORK, owork, IWORK, strideIWORK, oiwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlaebz;
