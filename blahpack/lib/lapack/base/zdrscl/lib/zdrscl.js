

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {NonNegativeInteger} N - N
* @param {number} sa - sa
* @param {Complex128Array} x - x
* @param {integer} strideX - strideX
* @returns {*} result
*/
function zdrscl( N, sa, x, strideX ) {
	var ox;

	ox = stride2offset( N, strideX );
	return base( N, sa, x, strideX, ox );
}


// EXPORTS //

module.exports = zdrscl;
