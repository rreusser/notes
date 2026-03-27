

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} trana - trana
* @param {string} tranb - tranb
* @param {integer} isgn - isgn
* @param {NonNegativeInteger} M - M
* @param {NonNegativeInteger} N - N
* @param {Float64Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} B - B
* @param {PositiveInteger} LDB - leading dimension of `B`
* @param {Float64Array} C - C
* @param {PositiveInteger} LDC - leading dimension of `C`
* @param {Float64Array} scale - scale
* @returns {*} result
*/
function dtrsyl( trana, tranb, isgn, M, N, A, LDA, B, LDB, C, LDC, scale ) { // eslint-disable-line max-len, max-params
	var sa1;
	var sa2;
	var sb1;
	var sb2;
	var sc1;
	var sc2;

	sa1 = 1;
	sa2 = LDA;
	sb1 = 1;
	sb2 = LDB;
	sc1 = 1;
	sc2 = LDC;
	return base( trana, tranb, isgn, M, N, A, sa1, sa2, 0, B, sb1, sb2, 0, C, sc1, sc2, 0, scale );
}


// EXPORTS //

module.exports = dtrsyl;
