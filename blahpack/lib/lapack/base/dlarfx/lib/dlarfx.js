
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var isOperationSide = require( '@stdlib/blas/base/assert/is-operation-side' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} side - side
* @param {NonNegativeInteger} M - M
* @param {NonNegativeInteger} N - N
* @param {Float64Array} v - v
* @param {integer} strideV - strideV
* @param {number} tau - tau
* @param {Float64Array} C - C
* @param {PositiveInteger} LDC - leading dimension of `C`
* @param {Float64Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @returns {*} result
*/
function dlarfx( side, M, N, v, strideV, tau, C, LDC, WORK, strideWORK ) { // eslint-disable-line max-len, max-params
	var owork;
	var sc1;
	var sc2;
	var ov;

	sc1 = 1;
	sc2 = LDC;
	ov = stride2offset( N, strideV );
	owork = stride2offset( N, strideWORK );
	if ( !isOperationSide( side ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid operation side. Value: `%s`.', side ) );
	}
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Third argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	return base( side, M, N, v, strideV, ov, tau, C, sc1, sc2, 0, WORK, strideWORK, owork );
}


// EXPORTS //

module.exports = dlarfx;
