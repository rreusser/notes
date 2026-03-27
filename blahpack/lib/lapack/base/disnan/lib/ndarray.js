/**
 * Tests whether a double-precision floating-point number is NaN.
 *
 *
 * @param {number} din - value to test
 * @returns {boolean} true if NaN, false otherwise
 */

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Tests whether a double-precision floating-point number is NaN.
*
* @param {number} din - value to test
* @returns {boolean} true if NaN, false otherwise
*/
function disnan( din ) {
	return base( din );
}


// EXPORTS //

module.exports = disnan;
