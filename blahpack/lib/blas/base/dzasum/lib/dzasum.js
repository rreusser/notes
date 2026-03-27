

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {NonNegativeInteger} N - N
* @param {Complex128Array} zx - zx
* @param {integer} strideX - strideX
* @param {NonNegativeInteger} offsetX - offsetX
* @returns {*} result
*/
function dzasum( N, zx, strideX, offsetX ) {
	return base( N, zx, strideX, offsetX );
}


// EXPORTS //

module.exports = dzasum;
