
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {boolean} forwrd - forwrd
* @param {NonNegativeInteger} M - M
* @param {NonNegativeInteger} N - N
* @param {Float64Array} X - X
* @param {PositiveInteger} LDX - leading dimension of `X`
* @param {Int32Array} k - k
* @param {integer} strideK - strideK
* @returns {*} result
*/
function dlapmt( forwrd, M, N, X, LDX, k, strideK ) { // eslint-disable-line max-len, max-params
	var sx1;
	var sx2;
	var ok;

	sx1 = 1;
	sx2 = LDX;
	ok = stride2offset( N, strideK );
	return base( forwrd, M, N, X, sx1, sx2, 0, k, strideK, ok );
}


// EXPORTS //

module.exports = dlapmt;
