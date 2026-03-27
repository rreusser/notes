

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
* @param {Float64Array} TAU - TAU
* @param {integer} strideTAU - strideTAU
* @param {Float64Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @returns {*} result
*/
function dgerqf( M, N, A, LDA, TAU, strideTAU, WORK, strideWORK ) { // eslint-disable-line max-len, max-params
	var owork;
	var otau;
	var sa1;
	var sa2;

	sa1 = 1;
	sa2 = LDA;
	otau = stride2offset( N, strideTAU );
	owork = stride2offset( N, strideWORK );
	return base( M, N, A, sa1, sa2, 0, TAU, strideTAU, otau, WORK, strideWORK, owork );
}


// EXPORTS //

module.exports = dgerqf;
