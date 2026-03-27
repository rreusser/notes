

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} jobvs - jobvs
* @param {string} sort - sort
* @param {Function} select - select
* @param {NonNegativeInteger} N - N
* @param {Complex128Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} sdim - sdim
* @param {Complex128Array} W - W
* @param {integer} strideW - strideW
* @param {Complex128Array} VS - VS
* @param {PositiveInteger} LDVS - leading dimension of `VS`
* @param {Complex128Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @param {integer} lwork - lwork
* @param {Float64Array} RWORK - RWORK
* @param {integer} strideRWORK - strideRWORK
* @param {Uint8Array} BWORK - BWORK
* @param {integer} strideBWORK - strideBWORK
* @returns {*} result
*/
function zgees( jobvs, sort, select, N, A, LDA, sdim, W, strideW, VS, LDVS, WORK, strideWORK, lwork, RWORK, strideRWORK, BWORK, strideBWORK ) { // eslint-disable-line max-len, max-params
	var obwork;
	var orwork;
	var owork;
	var svs1;
	var svs2;
	var sa1;
	var sa2;
	var ow;

	sa1 = 1;
	sa2 = LDA;
	svs1 = 1;
	svs2 = LDVS;
	ow = stride2offset( N, strideW );
	owork = stride2offset( N, strideWORK );
	orwork = stride2offset( N, strideRWORK );
	obwork = stride2offset( N, strideBWORK );
	return base( jobvs, sort, select, N, A, sa1, sa2, 0, sdim, W, strideW, ow, VS, svs1, svs2, 0, WORK, strideWORK, owork, lwork, RWORK, strideRWORK, orwork, BWORK, strideBWORK, obwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zgees;
