

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} uplo - uplo
* @param {NonNegativeInteger} N - N
* @param {NonNegativeInteger} nrhs - nrhs
* @param {Float64Array} d - d
* @param {integer} strideD - strideD
* @param {Complex128Array} e - e
* @param {integer} strideE - strideE
* @param {Complex128Array} B - B
* @param {PositiveInteger} LDB - leading dimension of `B`
* @returns {*} result
*/
function zpttrs( uplo, N, nrhs, d, strideD, e, strideE, B, LDB ) { // eslint-disable-line max-len, max-params
	var sb1;
	var sb2;
	var od;
	var oe;

	sb1 = 1;
	sb2 = LDB;
	od = stride2offset( N, strideD );
	oe = stride2offset( N, strideE );
	return base( uplo, N, nrhs, d, strideD, od, e, strideE, oe, B, sb1, sb2, 0 );
}


// EXPORTS //

module.exports = zpttrs;
