

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} uplo - uplo
* @param {string} trans - trans
* @param {string} diag - diag
* @param {NonNegativeInteger} N - N
* @param {Float64Array} AP - AP
* @param {integer} strideAP - strideAP
* @param {Float64Array} x - x
* @param {integer} strideX - strideX
* @returns {*} result
*/
function dtpsv( uplo, trans, diag, N, AP, strideAP, x, strideX ) { // eslint-disable-line max-len, max-params
	var oap;
	var ox;

	oap = stride2offset( N, strideAP );
	ox = stride2offset( N, strideX );
	return base( uplo, trans, diag, N, AP, strideAP, oap, x, strideX, ox );
}


// EXPORTS //

module.exports = dtpsv;
