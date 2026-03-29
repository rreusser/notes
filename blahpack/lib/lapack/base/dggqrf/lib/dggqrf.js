
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {NonNegativeInteger} N - N
* @param {NonNegativeInteger} M - M
* @param {NonNegativeInteger} p - p
* @param {Float64Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} TAUA - TAUA
* @param {integer} strideTAUA - strideTAUA
* @param {Float64Array} B - B
* @param {PositiveInteger} LDB - leading dimension of `B`
* @param {Float64Array} TAUB - TAUB
* @param {integer} strideTAUB - strideTAUB
* @param {Float64Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @param {integer} lwork - lwork
* @returns {*} result
*/
function dggqrf( N, M, p, A, LDA, TAUA, strideTAUA, B, LDB, TAUB, strideTAUB, WORK, strideWORK, lwork ) { // eslint-disable-line max-len, max-params
	var otaua;
	var otaub;
	var owork;
	var sa1;
	var sa2;
	var sb1;
	var sb2;

	sa1 = 1;
	sa2 = LDA;
	sb1 = 1;
	sb2 = LDB;
	otaua = stride2offset( N, strideTAUA );
	otaub = stride2offset( N, strideTAUB );
	owork = stride2offset( N, strideWORK );
	return base( N, M, p, A, sa1, sa2, 0, TAUA, strideTAUA, otaua, B, sb1, sb2, 0, TAUB, strideTAUB, otaub, WORK, strideWORK, owork, lwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dggqrf;
