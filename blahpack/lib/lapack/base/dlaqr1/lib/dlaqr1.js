
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* Sets a scalar multiple of the first column of the product.
*
* @param {NonNegativeInteger} N - N
* @param {Float64Array} H - H
* @param {PositiveInteger} LDH - leading dimension of `H`
* @param {number} sr1 - sr1
* @param {number} si1 - si1
* @param {number} sr2 - sr2
* @param {number} si2 - si2
* @param {Float64Array} v - v
* @param {integer} strideV - strideV
* @returns {*} result
*/
function dlaqr1( N, H, LDH, sr1, si1, sr2, si2, v, strideV ) { // eslint-disable-line max-len, max-params
	var sh1;
	var sh2;
	var ov;

	sh1 = 1;
	sh2 = LDH;
	ov = stride2offset( N, strideV );
	return base( N, H, sh1, sh2, 0, sr1, si1, sr2, si2, v, strideV, ov );
}


// EXPORTS //

module.exports = dlaqr1;
