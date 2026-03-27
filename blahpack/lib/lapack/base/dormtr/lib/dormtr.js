

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} side - side
* @param {string} uplo - uplo
* @param {string} trans - trans
* @param {NonNegativeInteger} M - M
* @param {NonNegativeInteger} N - N
* @param {Float64Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} TAU - TAU
* @param {integer} strideTAU - strideTAU
* @param {Float64Array} C - C
* @param {PositiveInteger} LDC - leading dimension of `C`
* @param {Float64Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @param {integer} lwork - lwork
* @returns {*} result
*/
function dormtr( side, uplo, trans, M, N, A, LDA, TAU, strideTAU, C, LDC, WORK, strideWORK, lwork ) { // eslint-disable-line max-len, max-params
	var owork;
	var otau;
	var sa1;
	var sa2;
	var sc1;
	var sc2;

	sa1 = 1;
	sa2 = LDA;
	sc1 = 1;
	sc2 = LDC;
	otau = stride2offset( N, strideTAU );
	owork = stride2offset( N, strideWORK );
	return base( side, uplo, trans, M, N, A, sa1, sa2, 0, TAU, strideTAU, otau, C, sc1, sc2, 0, WORK, strideWORK, owork, lwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dormtr;
