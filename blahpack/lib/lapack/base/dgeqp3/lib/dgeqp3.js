
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
* @param {Int32Array} JPVT - JPVT
* @param {integer} strideJPVT - strideJPVT
* @param {Float64Array} TAU - TAU
* @param {integer} strideTAU - strideTAU
* @returns {*} result
*/
function dgeqp3( M, N, A, LDA, JPVT, strideJPVT, TAU, strideTAU ) { // eslint-disable-line max-len, max-params
	var ojpvt;
	var otau;
	var sa1;
	var sa2;

	sa1 = 1;
	sa2 = LDA;
	ojpvt = stride2offset( N, strideJPVT );
	otau = stride2offset( N, strideTAU );
	return base( M, N, A, sa1, sa2, 0, JPVT, strideJPVT, ojpvt, TAU, strideTAU, otau );
}


// EXPORTS //

module.exports = dgeqp3;
