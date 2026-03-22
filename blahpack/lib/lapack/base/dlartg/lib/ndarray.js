

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Generates a plane rotation (real Givens rotation).
*
* @param {number} f - first component of the vector to be rotated
* @param {number} g - second component of the vector to be rotated
* @returns {Object} object with properties `c`, `s`, and `r`
*/
function dlartg( f, g ) {
	return base( f, g );
}


// EXPORTS //

module.exports = dlartg;
