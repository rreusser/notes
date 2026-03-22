

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Compute eigenvalues of a 2-by-2 symmetric matrix
*
* @param {number} a - a
* @param {number} b - b
* @param {number} c - c
* @param {number} rt1 - rt1
* @param {number} rt2 - rt2
*/
function dlae2( a, b, c, rt1, rt2 ) {
	return base( a, b, c, rt1, rt2 );
}


// EXPORTS //

module.exports = dlae2;
