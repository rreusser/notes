/**
 * Performs complex division: out = X / Y, where X and Y are complex.
 *
 * The computation will not overflow on an intermediary step unless
 * the result overflows.
 *
 *
 * @param {Float64Array} x - numerator complex number [real, imag]
 * @param {Float64Array} y - denominator complex number [real, imag]
 * @param {Float64Array} out - output complex number [real, imag]
 * @returns {Float64Array} out
 */

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Performs complex division: out = X / Y, where X and Y are complex.
*
* @param {Float64Array} x - numerator complex number [real, imag]
* @param {Float64Array} y - denominator complex number [real, imag]
* @param {Float64Array} out - output complex number [real, imag]
* @returns {Float64Array} out
*/
function zladiv( x, y, out ) {
	return base( x, y, out );
}


// EXPORTS //

module.exports = zladiv;
