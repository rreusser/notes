

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} uplo - uplo
* @param {NonNegativeInteger} N - N
* @param {Complex128} alpha - alpha
* @param {Complex128Array} x - x
* @param {integer} strideX - strideX
* @param {Complex128Array} y - y
* @param {integer} strideY - strideY
* @param {Complex128Array} AP - AP
* @param {integer} strideAP - strideAP
* @returns {*} result
*/
function zhpr2( uplo, N, alpha, x, strideX, y, strideY, AP, strideAP ) { // eslint-disable-line max-len, max-params
	var oap;
	var ox;
	var oy;

	ox = stride2offset( N, strideX );
	oy = stride2offset( N, strideY );
	oap = stride2offset( N, strideAP );
	return base( uplo, N, alpha, x, strideX, ox, y, strideY, oy, AP, strideAP, oap );
}


// EXPORTS //

module.exports = zhpr2;
