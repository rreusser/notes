

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Construct a Givens plane rotation
*
* @param {number} a - a
* @param {number} b - b
* @param {number} c - c
* @param {number} s - s
*/
function drotg( a, b, c, s ) {
	return base( a, b, c, s );
}


// EXPORTS //

module.exports = drotg;
