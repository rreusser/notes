
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* Applies an elementary reflector H to a complex M-by-N matrix C.
*
* @param {string} side - `'left'` or `'right'`
* @param {NonNegativeInteger} M - number of rows of C
* @param {NonNegativeInteger} N - number of columns of C
* @param {Complex128Array} v - the vector v in the reflector
* @param {integer} strideV - stride for v (in complex elements)
* @param {Complex128} tau - the complex scalar tau
* @param {Complex128Array} C - the M-by-N matrix
* @param {PositiveInteger} LDC - leading dimension of `C`
* @param {Complex128Array} WORK - workspace
* @param {integer} strideWORK - stride for WORK (in complex elements)
* @returns {void}
*/
function zlarfx( side, M, N, v, strideV, tau, C, LDC, WORK, strideWORK ) { // eslint-disable-line max-len, max-params
	var owork;
	var sc1;
	var sc2;
	var ov;

	sc1 = 1;
	sc2 = LDC;
	ov = stride2offset( N, strideV );
	owork = stride2offset( N, strideWORK );
	return base( side, M, N, v, strideV, ov, tau, C, sc1, sc2, 0, WORK, strideWORK, owork ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zlarfx;
