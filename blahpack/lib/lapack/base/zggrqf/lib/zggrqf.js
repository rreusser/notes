
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* Computes a generalized RQ factorization of an M-by-N matrix A and a P-by-N matrix B.
*
* @param {NonNegativeInteger} M - number of rows of A
* @param {NonNegativeInteger} p - number of rows of B
* @param {NonNegativeInteger} N - number of columns of A and B
* @param {Complex128Array} A - M-by-N matrix
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Complex128Array} TAUA - scalar factors for Q
* @param {integer} strideTAUA - stride for TAUA
* @param {Complex128Array} B - P-by-N matrix
* @param {PositiveInteger} LDB - leading dimension of `B`
* @param {Complex128Array} TAUB - scalar factors for Z
* @param {integer} strideTAUB - stride for TAUB
* @param {Complex128Array} WORK - workspace
* @param {integer} strideWORK - stride for WORK
* @param {integer} lwork - workspace size
* @returns {integer} status code (0 = success)
*/
function zggrqf( M, p, N, A, LDA, TAUA, strideTAUA, B, LDB, TAUB, strideTAUB, WORK, strideWORK, lwork ) { // eslint-disable-line max-len, max-params
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
	otaua = stride2offset( Math.min( M, N ), strideTAUA );
	otaub = stride2offset( Math.min( p, N ), strideTAUB );
	owork = stride2offset( lwork, strideWORK );
	return base( M, p, N, A, sa1, sa2, 0, TAUA, strideTAUA, otaua, B, sb1, sb2, 0, TAUB, strideTAUB, otaub, WORK, strideWORK, owork, lwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zggrqf;
