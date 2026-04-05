
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} jobvs - jobvs
* @param {string} sort - sort
* @param {Function} select - select
* @param {NonNegativeInteger} N - N
* @param {Float64Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} sdim - sdim
* @param {Float64Array} WR - WR
* @param {integer} strideWR - strideWR
* @param {Float64Array} WI - WI
* @param {integer} strideWI - strideWI
* @param {Float64Array} VS - VS
* @param {PositiveInteger} LDVS - leading dimension of `VS`
* @param {Float64Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @param {integer} lwork - lwork
* @param {Uint8Array} BWORK - BWORK
* @param {integer} strideBWORK - strideBWORK
* @returns {*} result
*/
function dgees( jobvs, sort, select, N, A, LDA, sdim, WR, strideWR, WI, strideWI, VS, LDVS, WORK, strideWORK, lwork, BWORK, strideBWORK ) { // eslint-disable-line max-len, max-params
	var obwork;
	var owork;
	var svs1;
	var svs2;
	var owi;
	var owr;
	var sa1;
	var sa2;

	sa1 = 1;
	sa2 = LDA;
	svs1 = 1;
	svs2 = LDVS;
	owr = stride2offset( N, strideWR );
	owi = stride2offset( N, strideWI );
	owork = stride2offset( N, strideWORK );
	obwork = stride2offset( N, strideBWORK );
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	return base( jobvs, sort, select, N, A, sa1, sa2, 0, sdim, WR, strideWR, owr, WI, strideWI, owi, VS, svs1, svs2, 0, WORK, strideWORK, owork, lwork, BWORK, strideBWORK, obwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dgees;
