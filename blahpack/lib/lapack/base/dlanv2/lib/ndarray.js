

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes the Schur factorization of a 2x2 nonsymmetric matrix
*
* @param {number} a - a
* @param {number} b - b
* @param {number} c - c
* @param {number} d - d
* @param {number} rt1r - rt1r
* @param {number} rt1i - rt1i
* @param {number} rt2r - rt2r
* @param {number} rt2i - rt2i
* @param {number} cs - cs
* @param {number} sn - sn
*/
function dlanv2( a, b, c, d, rt1r, rt1i, rt2r, rt2i, cs, sn ) { // eslint-disable-line max-len, max-params
	return base( a, b, c, d, rt1r, rt1i, rt2r, rt2i, cs, sn ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlanv2;
