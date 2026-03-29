
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* Solves a complex general tridiagonal system of linear equations A * X = B.
*
* @param {NonNegativeInteger} N - N
* @param {integer} nrhs - nrhs
* @param {Complex128Array} DL - DL
* @param {integer} strideDL - strideDL
* @param {Complex128Array} d - d
* @param {integer} strideD - strideD
* @param {Complex128Array} DU - DU
* @param {integer} strideDU - strideDU
* @param {Complex128Array} B - B
* @param {PositiveInteger} LDB - leading dimension of `B`
* @returns {*} result
*/
function zgtsv( N, nrhs, DL, strideDL, d, strideD, DU, strideDU, B, LDB ) { // eslint-disable-line max-len, max-params
	var odl;
	var odu;
	var sb1;
	var sb2;
	var od;

	sb1 = 1;
	sb2 = LDB;
	odl = stride2offset( N, strideDL );
	od = stride2offset( N, strideD );
	odu = stride2offset( N, strideDU );
	return base( N, nrhs, DL, strideDL, odl, d, strideD, od, DU, strideDU, odu, B, sb1, sb2, 0 );
}


// EXPORTS //

module.exports = zgtsv;
