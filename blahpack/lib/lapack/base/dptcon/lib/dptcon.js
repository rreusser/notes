
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
* @param {Float64Array} e - e
* @param {integer} strideE - strideE
* @param {number} anorm - anorm
* @param {Float64Array} rcond - rcond
* @param {Float64Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @returns {*} result
*/
function dptcon( N, d, strideD, e, strideE, anorm, rcond, WORK, strideWORK ) { // eslint-disable-line max-len, max-params
	var owork;
	var od;
	var oe;

	od = stride2offset( N, strideD );
	oe = stride2offset( N, strideE );
	owork = stride2offset( N, strideWORK );
	return base( N, d, strideD, od, e, strideE, oe, anorm, rcond, WORK, strideWORK, owork );
}


// EXPORTS //

module.exports = dptcon;
