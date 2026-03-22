

'use strict';

// MODULES //

var base = require( './base.js' );


// VARIABLES //

var out = new Float64Array( 3 );


// MAIN //

/**
* Generates a plane rotation (real Givens rotation).
*
* @param {number} f - first component of the vector to be rotated
* @param {number} g - second component of the vector to be rotated
* @returns {Object} object with properties `c`, `s`, and `r`
*/
function dlartg( f, g ) {
	base( f, g, out );
	return {
		'c': out[ 0 ],
		's': out[ 1 ],
		'r': out[ 2 ]
	};
}


// EXPORTS //

module.exports = dlartg;
