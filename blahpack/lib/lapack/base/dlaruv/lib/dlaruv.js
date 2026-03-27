

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {Int32Array} iseed - iseed
* @param {integer} strideISEED - strideISEED
* @param {NonNegativeInteger} N - N
* @param {Float64Array} x - x
* @param {integer} strideX - strideX
* @returns {*} result
*/
function dlaruv( iseed, strideISEED, N, x, strideX ) {
	var oiseed;
	var ox;

	oiseed = stride2offset( N, strideISEED );
	ox = stride2offset( N, strideX );
	return base( iseed, strideISEED, oiseed, N, x, strideX, ox );
}


// EXPORTS //

module.exports = dlaruv;
