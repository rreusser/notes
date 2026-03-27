

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} uplo - uplo
* @param {NonNegativeInteger} N - N
* @param {NonNegativeInteger} kd - kd
* @param {NonNegativeInteger} nrhs - nrhs
* @param {Complex128Array} AB - AB
* @param {PositiveInteger} LDAB - leading dimension of `AB`
* @param {Complex128Array} B - B
* @param {PositiveInteger} LDB - leading dimension of `B`
* @returns {*} result
*/
function zpbsv( uplo, N, kd, nrhs, AB, LDAB, B, LDB ) { // eslint-disable-line max-len, max-params
	var sab1;
	var sab2;
	var sb1;
	var sb2;

	sab1 = 1;
	sab2 = LDAB;
	sb1 = 1;
	sb2 = LDB;
	return base( uplo, N, kd, nrhs, AB, sab1, sab2, 0, B, sb1, sb2, 0 );
}


// EXPORTS //

module.exports = zpbsv;
