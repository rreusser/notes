

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Return sqrt(x**2 + y**2), taking care not to cause unnecessary overflow.
*
* @param {number} x - x
* @param {number} y - y
* @returns {number} result
*/
function dlapy2( x, y ) {
	return base( x, y );
}


// EXPORTS //

module.exports = dlapy2;
