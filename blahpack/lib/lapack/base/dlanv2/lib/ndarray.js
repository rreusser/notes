
'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Returns |a| with the sign of b (Fortran SIGN intrinsic).
*
* @param {number} a - magnitude source
* @param {number} b - sign source
* @returns {number} |a| * sign(b)
*/
function dlanv2( a, b, c, d, rt1r, rt1i, rt2r, rt2i, cs, sn ) { // eslint-disable-line max-len, max-params
	return base( a, b, c, d, rt1r, rt1i, rt2r, rt2i, cs, sn ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlanv2;
