

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {NonNegativeInteger} N - N
* @param {Complex128Array} V - V
* @param {integer} strideV - strideV
* @param {Complex128Array} X - X
* @param {integer} strideX - strideX
* @param {Float64Array} EST - EST
* @param {Int32Array} KASE - KASE
* @param {Int32Array} ISAVE - ISAVE
* @param {integer} strideISAVE - strideISAVE
* @returns {*} result
*/
function zlacn2( N, V, strideV, X, strideX, EST, KASE, ISAVE, strideISAVE ) { // eslint-disable-line max-len, max-params
	var oisave;
	var ov;
	var ox;

	ov = stride2offset( N, strideV );
	ox = stride2offset( N, strideX );
	oisave = stride2offset( N, strideISAVE );
	return base( N, V, strideV, ov, X, strideX, ox, EST, KASE, ISAVE, strideISAVE, oisave );
}


// EXPORTS //

module.exports = zlacn2;
