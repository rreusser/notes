
'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Returns `re*re + im*im` without complex abs overhead.
*
* @param {number} re - real part
* @param {number} im - imaginary part
* @returns {number} sum of squares
*/
function zrotg( a, b, c, s ) {
	return base( a, b, c, s );
}


// EXPORTS //

module.exports = zrotg;
