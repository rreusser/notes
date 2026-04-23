/**
 * Computes the sum of the absolute values of the real and imaginary parts of a double-precision complex number.
 *
 *
 * @param {Float64Array} z - complex number [real, imag]
 * @returns {number} |Re(z)| + |Im(z)|
 */

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes the sum of the absolute values of the real and imaginary parts of a double-precision complex number.
*
* @param {Float64Array} z - complex number [real, imag]
* @returns {number} |Re(z)| + |Im(z)|
*/
function dcabs1( z ) {
	return base( z );
}


// EXPORTS //

module.exports = dcabs1;
