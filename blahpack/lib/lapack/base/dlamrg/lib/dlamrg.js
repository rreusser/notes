
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {integer} n1 - n1
* @param {integer} n2 - n2
* @param {Float64Array} a - a
* @param {integer} strideA - strideA
* @param {integer} dtrd1 - dtrd1
* @param {integer} dtrd2 - dtrd2
* @param {Int32Array} INDEX - INDEX
* @param {integer} strideINDEX - strideINDEX
* @returns {*} result
*/
function dlamrg( n1, n2, a, strideA, dtrd1, dtrd2, INDEX, strideINDEX ) { // eslint-disable-line max-len, max-params
	var oindex;
	var oa;

	oa = stride2offset( N, strideA );
	oindex = stride2offset( N, strideINDEX );
	return base( n1, n2, a, strideA, oa, dtrd1, dtrd2, INDEX, strideINDEX, oindex );
}


// EXPORTS //

module.exports = dlamrg;
