
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} job - job
* @param {string} compz - compz
* @param {NonNegativeInteger} N - N
* @param {integer} ilo - ilo
* @param {integer} ihi - ihi
* @param {Float64Array} H - H
* @param {PositiveInteger} LDH - leading dimension of `H`
* @param {Float64Array} WR - WR
* @param {integer} strideWR - strideWR
* @param {Float64Array} WI - WI
* @param {integer} strideWI - strideWI
* @param {Float64Array} Z - Z
* @param {PositiveInteger} LDZ - leading dimension of `Z`
* @returns {*} result
*/
function dhseqr( job, compz, N, ilo, ihi, H, LDH, WR, strideWR, WI, strideWI, Z, LDZ ) { // eslint-disable-line max-len, max-params
	var owi;
	var owr;
	var sh1;
	var sh2;
	var sz1;
	var sz2;

	sh1 = 1;
	sh2 = LDH;
	sz1 = 1;
	sz2 = LDZ;
	owr = stride2offset( N, strideWR );
	owi = stride2offset( N, strideWI );
	return base( job, compz, N, ilo, ihi, H, sh1, sh2, 0, WR, strideWR, owr, WI, strideWI, owi, Z, sz1, sz2, 0 ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dhseqr;
