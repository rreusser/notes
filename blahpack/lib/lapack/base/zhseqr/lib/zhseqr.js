

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
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
* @param {Complex128Array} H - H
* @param {PositiveInteger} LDH - leading dimension of `H`
* @param {Complex128Array} w - w
* @param {integer} strideW - strideW
* @param {Complex128Array} Z - Z
* @param {PositiveInteger} LDZ - leading dimension of `Z`
* @param {Complex128Array} WORK - WORK
* @param {integer} strideWORK - strideWORK
* @param {integer} lwork - lwork
* @returns {*} result
*/
function zhseqr( job, compz, N, ilo, ihi, H, LDH, w, strideW, Z, LDZ, WORK, strideWORK, lwork ) { // eslint-disable-line max-len, max-params
	var owork;
	var sh1;
	var sh2;
	var sz1;
	var sz2;
	var ow;

	sh1 = 1;
	sh2 = LDH;
	sz1 = 1;
	sz2 = LDZ;
	ow = stride2offset( N, strideW );
	owork = stride2offset( N, strideWORK );
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Third argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	return base( job, compz, N, ilo, ihi, H, sh1, sh2, 0, w, strideW, ow, Z, sz1, sz2, 0, WORK, strideWORK, owork, lwork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zhseqr;
