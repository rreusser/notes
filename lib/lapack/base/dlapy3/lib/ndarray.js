/**
 * Computes sqrt(x^2 + y^2 + z^2) safely, avoiding unnecessary overflow.
 * and underflow.
 *
 *
 * @param {number} x - first value
 * @param {number} y - second value
 * @param {number} z - third value
 * @returns {number} sqrt(x^2 + y^2 + z^2)
 */

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes sqrt(x^2 + y^2 + z^2) safely, avoiding unnecessary overflow.
*
* @param {number} x - first value
* @param {number} y - second value
* @param {number} z - third value
* @returns {number} sqrt(x^2 + y^2 + z^2)
*/
function dlapy3( x, y, z ) {
	return base( x, y, z );
}


// EXPORTS //

module.exports = dlapy3;
