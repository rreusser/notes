
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {NonNegativeInteger} N - N
* @param {Float64Array} x - x
* @param {integer} strideX - strideX
* @param {Float64Array} y - y
* @param {integer} strideY - strideY
* @returns {*} result
*/
function dswap( N, x, strideX, y, strideY ) {
	var ox;
	var oy;

	ox = stride2offset( N, strideX );
	oy = stride2offset( N, strideY );
	return base( N, x, strideX, ox, y, strideY, oy );
}


// EXPORTS //

module.exports = dswap;
