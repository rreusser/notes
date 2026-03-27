

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} jobu - jobu
* @param {string} jobv - jobv
* @param {string} jobq - jobq
* @param {NonNegativeInteger} M - M
* @param {NonNegativeInteger} p - p
* @param {NonNegativeInteger} N - N
* @param {Float64Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} B - B
* @param {PositiveInteger} LDB - leading dimension of `B`
* @param {number} tola - tola
* @param {number} tolb - tolb
* @param {Array} K - K
* @param {Array} l - l
* @param {Float64Array} U - U
* @param {PositiveInteger} LDU - leading dimension of `U`
* @param {Float64Array} V - V
* @param {PositiveInteger} LDV - leading dimension of `V`
* @param {Float64Array} Q - Q
* @param {PositiveInteger} LDQ - leading dimension of `Q`
* @param {Int32Array} IWORK - IWORK
* @param {integer} strideIWORK - strideIWORK
* @param {Float64Array} TAU - TAU
* @param {integer} strideTAU - strideTAU
* @param {Float64Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @param {integer} lwork - lwork
* @returns {*} result
*/
function dggsvp3( jobu, jobv, jobq, M, p, N, A, LDA, B, LDB, tola, tolb, K, l, U, LDU, V, LDV, Q, LDQ, IWORK, strideIWORK, TAU, strideTAU, WORK, strideWORK, lwork ) { // eslint-disable-line max-len, max-params
	var oiwork;
	var owork;
	var otau;
	var sa1;
	var sa2;
	var sb1;
	var sb2;
	var sq1;
	var sq2;
	var su1;
	var su2;
	var sv1;
	var sv2;

	sa1 = 1;
	sa2 = LDA;
	sb1 = 1;
	sb2 = LDB;
	su1 = 1;
	su2 = LDU;
	sv1 = 1;
	sv2 = LDV;
	sq1 = 1;
	sq2 = LDQ;
	oiwork = stride2offset( N, strideIWORK );
	otau = stride2offset( N, strideTAU );
	owork = stride2offset( N, strideWORK );
	return base( jobu, jobv, jobq, M, p, N, A, sa1, sa2, 0, B, sb1, sb2, 0, tola, tolb, K, l, U, su1, su2, 0, V, sv1, sv2, 0, Q, sq1, sq2, 0, IWORK, strideIWORK, oiwork, TAU, strideTAU, otau, WORK, strideWORK, owork, lwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dggsvp3;
