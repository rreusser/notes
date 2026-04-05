
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} job - job
* @param {string} compq - compq
* @param {Uint8Array|Array} SELECT - SELECT
* @param {integer} strideSELECT - strideSELECT
* @param {NonNegativeInteger} N - N
* @param {Float64Array} T - T
* @param {PositiveInteger} LDT - leading dimension of `T`
* @param {Float64Array} Q - Q
* @param {PositiveInteger} LDQ - leading dimension of `Q`
* @param {Float64Array} WR - WR
* @param {integer} strideWR - strideWR
* @param {Float64Array} WI - WI
* @param {integer} strideWI - strideWI
* @param {Float64Array} M - M
* @param {Float64Array} s - s
* @param {Float64Array} sep - sep
* @param {Float64Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @param {integer} lwork - lwork
* @param {Int32Array} IWORK - IWORK
* @param {integer} strideIWORK - strideIWORK
* @param {integer} liwork - liwork
* @returns {*} result
*/
function dtrsen( job, compq, SELECT, strideSELECT, N, T, LDT, Q, LDQ, WR, strideWR, WI, strideWI, M, s, sep, WORK, strideWORK, lwork, IWORK, strideIWORK, liwork ) { // eslint-disable-line max-len, max-params
	var oselect;
	var oiwork;
	var owork;
	var owi;
	var owr;
	var sq1;
	var sq2;
	var st1;
	var st2;

	st1 = 1;
	st2 = LDT;
	sq1 = 1;
	sq2 = LDQ;
	oselect = stride2offset( N, strideSELECT );
	owr = stride2offset( N, strideWR );
	owi = stride2offset( N, strideWI );
	owork = stride2offset( N, strideWORK );
	oiwork = stride2offset( N, strideIWORK );
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Fifth argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. Fourteenth argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	return base( job, compq, SELECT, strideSELECT, oselect, N, T, st1, st2, 0, Q, sq1, sq2, 0, WR, strideWR, owr, WI, strideWI, owi, M, s, sep, WORK, strideWORK, owork, lwork, IWORK, strideIWORK, oiwork, liwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dtrsen;
