

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Construct a Givens plane rotation with real cosine and complex sine.
*
* @param {number} a - a
* @param {number} b - b
* @param {number} c - c
* @param {number} s - s
*/
function zrotg( a, b, c, s ) {
	return base( a, b, c, s );
}


// EXPORTS //

module.exports = zrotg;
