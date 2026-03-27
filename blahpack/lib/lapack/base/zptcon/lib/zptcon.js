

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {NonNegativeInteger} N - N
* @param {Float64Array} d - d
* @param {integer} strideD - strideD
* @param {Complex128Array} e - e
* @param {integer} strideE - strideE
* @param {number} anorm - anorm
* @param {Float64Array} rcond - rcond
* @param {Float64Array} RWORK - RWORK
* @param {integer} strideRWORK - strideRWORK
* @returns {*} result
*/
function zptcon( N, d, strideD, e, strideE, anorm, rcond, RWORK, strideRWORK ) { // eslint-disable-line max-len, max-params
	var orwork;
	var od;
	var oe;

	od = stride2offset( N, strideD );
	oe = stride2offset( N, strideE );
	orwork = stride2offset( N, strideRWORK );
	return base( N, d, strideD, od, e, strideE, oe, anorm, rcond, RWORK, strideRWORK, orwork );
}


// EXPORTS //

module.exports = zptcon;
