

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
* @param {NonNegativeInteger} offset - offset
* @param {NonNegativeInteger} nb - nb
* @param {Float64Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Int32Array} JPVT - JPVT
* @param {integer} strideJPVT - strideJPVT
* @param {Float64Array} TAU - TAU
* @param {integer} strideTAU - strideTAU
* @param {Float64Array} VN1 - VN1
* @param {integer} strideVN1 - strideVN1
* @param {Float64Array} VN2 - VN2
* @param {integer} strideVN2 - strideVN2
* @param {Float64Array} AUXV - AUXV
* @param {integer} strideAUXV - strideAUXV
* @param {Float64Array} F - F
* @param {PositiveInteger} LDF - leading dimension of `F`
* @returns {*} result
*/
function dlaqps( M, N, offset, nb, A, LDA, JPVT, strideJPVT, TAU, strideTAU, VN1, strideVN1, VN2, strideVN2, AUXV, strideAUXV, F, LDF ) { // eslint-disable-line max-len, max-params
	var oauxv;
	var ojpvt;
	var otau;
	var ovn1;
	var ovn2;
	var sa1;
	var sa2;
	var sf1;
	var sf2;

	sa1 = 1;
	sa2 = LDA;
	sf1 = 1;
	sf2 = LDF;
	ojpvt = stride2offset( N, strideJPVT );
	otau = stride2offset( N, strideTAU );
	ovn1 = stride2offset( N, strideVN1 );
	ovn2 = stride2offset( N, strideVN2 );
	oauxv = stride2offset( N, strideAUXV );
	return base( M, N, offset, nb, A, sa1, sa2, 0, JPVT, strideJPVT, ojpvt, TAU, strideTAU, otau, VN1, strideVN1, ovn1, VN2, strideVN2, ovn2, AUXV, strideAUXV, oauxv, F, sf1, sf2, 0 ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlaqps;
