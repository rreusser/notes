
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {boolean} wantq - wantq
* @param {NonNegativeInteger} N - N
* @param {Float64Array} T - T
* @param {PositiveInteger} LDT - leading dimension of `T`
* @param {Float64Array} Q - Q
* @param {PositiveInteger} LDQ - leading dimension of `Q`
* @param {integer} j1 - j1
* @param {integer} n1 - n1
* @param {integer} n2 - n2
* @param {Float64Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @returns {*} result
*/
function dlaexc( wantq, N, T, LDT, Q, LDQ, j1, n1, n2, WORK, strideWORK ) { // eslint-disable-line max-len, max-params
	var owork;
	var sq1;
	var sq2;
	var st1;
	var st2;

	st1 = 1;
	st2 = LDT;
	sq1 = 1;
	sq2 = LDQ;
	owork = stride2offset( N, strideWORK );
	return base( wantq, N, T, st1, st2, 0, Q, sq1, sq2, 0, j1, n1, n2, WORK, strideWORK, owork );
}


// EXPORTS //

module.exports = dlaexc;
