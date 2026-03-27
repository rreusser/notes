

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
* @param {number} alpha - alpha
* @param {Float64Array} x - x
* @param {integer} strideX - strideX
* @param {Float64Array} AP - AP
* @param {integer} strideAP - strideAP
* @returns {*} result
*/
function dspr( uplo, N, alpha, x, strideX, AP, strideAP ) { // eslint-disable-line max-len, max-params
	var oap;
	var ox;

	ox = stride2offset( N, strideX );
	oap = stride2offset( N, strideAP );
	return base( uplo, N, alpha, x, strideX, ox, AP, strideAP, oap );
}


// EXPORTS //

module.exports = dspr;
