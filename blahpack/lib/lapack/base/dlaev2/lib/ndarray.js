

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Compute eigendecomposition of a 2-by-2 symmetric matrix
*
* @param {number} a - a
* @param {number} b - b
* @param {number} c - c
* @param {number} rt1 - rt1
* @param {number} rt2 - rt2
* @param {number} cs1 - cs1
* @param {number} sn1 - sn1
*/
function dlaev2( a, b, c, rt1, rt2, cs1, sn1 ) { // eslint-disable-line max-len, max-params
	return base( a, b, c, rt1, rt2, cs1, sn1 ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlaev2;
