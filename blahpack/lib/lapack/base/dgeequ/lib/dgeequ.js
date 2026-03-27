

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {NonNegativeInteger} M - M
* @param {NonNegativeInteger} N - N
* @param {Float64Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} r - r
* @param {integer} strideR - strideR
* @param {Float64Array} c - c
* @param {integer} strideC - strideC
* @returns {*} result
*/
function dgeequ( M, N, A, LDA, r, strideR, c, strideC ) { // eslint-disable-line max-len, max-params
	var sa1;
	var sa2;
	var oc;
	var or;

	sa1 = 1;
	sa2 = LDA;
	or = stride2offset( N, strideR );
	oc = stride2offset( N, strideC );
	return base( M, N, A, sa1, sa2, 0, r, strideR, or, c, strideC, oc );
}


// EXPORTS //

module.exports = dgeequ;
